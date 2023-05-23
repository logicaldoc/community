package com.logicaldoc.gui.frontend.client.document.note;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageDrawingPane;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.ArrowStyle;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.KnobType;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleRotationMode;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.drawing.CloseCommand;
import com.smartgwt.client.widgets.drawing.DrawGroup;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.drawing.DrawLabel;
import com.smartgwt.client.widgets.drawing.DrawLine;
import com.smartgwt.client.widgets.drawing.DrawOval;
import com.smartgwt.client.widgets.drawing.DrawRect;
import com.smartgwt.client.widgets.drawing.DrawShape;
import com.smartgwt.client.widgets.drawing.LineToCommand;
import com.smartgwt.client.widgets.drawing.MoveToCommand;
import com.smartgwt.client.widgets.drawing.Point;
import com.smartgwt.client.widgets.form.fields.SliderItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A superclass for those windows that display annotations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public abstract class AbstractAnnotationsWindow extends Window {

	protected SliderItem pageCursor;

	protected SliderItem zoomItem;

	protected VLayout bottom = new VLayout();

	protected GUIDocument document;

	protected ImageDrawingPane pageDrawingPane;

	/**
	 * List of all the notes in all the pages
	 */
	protected List<GUIDocumentNote> notes = new ArrayList<>();

	/**
	 * Map of the items in the current page
	 */
	protected Map<DrawItem, GUIDocumentNote> currentPageItems = new HashMap<>();

	protected String fileVersion = null;

	/**
	 * The collection note types this panel will display or null for any type
	 */
	protected Collection<String> types;

	/**
	 * Constructor
	 * 
	 * @param document the document to be displayed
	 * @param fileVersion file version specification, null to use the current
	 *        version
	 * @param types collection of annotation types
	 */
	public AbstractAnnotationsWindow(GUIDocument document, String fileVersion, Collection<String> types) {
		super();
		this.document = document;
		this.fileVersion = fileVersion;
		this.types = types;
		if (this.fileVersion == null)
			this.fileVersion = document.getFileVersion();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("annotations") + " - " + document.getFileName() + " v" + fileVersion);
		setWidth100();
		setHeight100();

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		DocumentService.Instance.get().getNotes(document.getId(), fileVersion, types,
				new AsyncCallback<GUIDocumentNote[]>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocumentNote[] nts) {
						notes.addAll(Arrays.asList(nts));
						initGUI();
					}
				});
	}

	protected void showAnnotations(int page) {
		currentPageItems.clear();
		for (GUIDocumentNote note : notes)
			if (note.getPage() == page) {
				DrawItem noteDrawItem = prepareAnnotationItem(note);
				if (noteDrawItem != null) {
					note.setShowKnobs(false);
					pageDrawingPane.addDrawItem(noteDrawItem, false);
					noteDrawItem.draw();
					currentPageItems.put(noteDrawItem, note);
				}
			}
	}

	/**
	 * Concrete implementations should provide a DwarItem representing the
	 * passed note
	 * 
	 * @param note the note to use to create the DrawingItem
	 * 
	 * @return The DrawItem displaying the note
	 */
	protected abstract DrawItem prepareAnnotationItem(GUIDocumentNote note);

	/**
	 * Saves the actual positions of the notes in the current page that were
	 * modified
	 */
	protected void captureNotesPosition() {
		if (pageDrawingPane == null || pageDrawingPane.getDrawItems() == null)
			return;

		DrawItem[] items = pageDrawingPane.getDrawItems();
		if (items == null)
			return;

		for (DrawItem item : items) {
			GUIDocumentNote note = currentPageItems.get(item);
			if (note == null)
				continue;

			if (item.getRotationAsDouble() != note.getRotation()) {
				note.setMovedOrResized(true);
				note.setRotation(item.getRotationAsDouble());
			}

			if (note.isMovedOrResized()) {
				if (item instanceof DrawLine) {
					DrawLine line = (DrawLine) item;
					note.setLeft(line.getStartLeftAsDouble() / (double) pageDrawingPane.getImageWidth());
					note.setTop(line.getStartTopAsDouble() / (double) pageDrawingPane.getImageHeight());
					note.setWidth(line.getEndLeftAsDouble() / (double) pageDrawingPane.getImageWidth());
					note.setHeight(line.getEndTopAsDouble() / (double) pageDrawingPane.getImageHeight());
				} else {
					Double[] box = item.getBoundingBoxAsDouble();
					note.setLeft(box[0] / (double) pageDrawingPane.getImageWidth());
					note.setTop(box[1] / (double) pageDrawingPane.getImageHeight());
					note.setWidth((box[2] - box[0]) / (double) pageDrawingPane.getImageWidth());
					note.setHeight((box[3] - box[1]) / (double) pageDrawingPane.getImageHeight());
				}
			}
		}
	}

	/**
	 * Saves all the annotations into the database
	 */
	protected void onSave() {
		captureNotesPosition();
		DocumentService.Instance.get().saveNotes(document.getId(), notes.toArray(new GUIDocumentNote[0]), types,
				new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						onNotesSaved();
					}
				});
	}

	protected void initGUI() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(event -> {
			DrawItem[] items = pageDrawingPane.getDrawItems();
			if (items != null)
				for (DrawItem item : items)
					item.hideAllKnobs();
			showPrintPreview(pageDrawingPane);
		});

		pageCursor = ItemFactory.newSliderItem("page", "page", 1.0, 1.0,
				document.getPreviewPages() > 0 ? document.getPreviewPages() : 1.0);
		pageCursor.setNumValues(document.getPreviewPages() > 0 ? document.getPreviewPages() : 1);
		pageCursor.addChangedHandler(event -> showCurrentPage());

		toolStrip.addFormItem(pageCursor);

		zoomItem = ItemFactory.newSliderItem("zoom", "zoom", 1.0, 0.1, 6.0);
		zoomItem.setTitle(I18N.message("zoom"));
		zoomItem.setNumValues(600);
		zoomItem.setRoundValues(false);
		zoomItem.setRoundPrecision(2);
		zoomItem.addChangedHandler(event -> pageDrawingPane.zoom((Double) event.getValue()));
		toolStrip.addFormItem(zoomItem);

		toolStrip.addSeparator();

		prepareAdditionalActions(toolStrip);

		toolStrip.addButton(print);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);

		bottom.setOverflow(Overflow.AUTO);
		addItem(bottom);

		showPage(1);
	}

	/**
	 * Invoked to populate the toolbar with additional actions
	 * 
	 * @param toolStrip The toolstrip to update
	 */
	protected abstract void prepareAdditionalActions(ToolStrip toolStrip);

	public ImageDrawingPane getPageDrawingPane() {
		return pageDrawingPane;
	}

	/**
	 * Invoked when the notes have been saved
	 */
	protected abstract void onNotesSaved();

	protected void showPage(int page) {
		if (pageDrawingPane != null) {
			captureNotesPosition();
			bottom.removeMembers(bottom.getMembers());
		}

		pageDrawingPane = new ImageDrawingPane(getPageUrl(page), null, imageElements -> {
			// Reload the document to update the pages count
			if (document.getPreviewPages() <= 1)
				DocumentService.Instance.get().getById(document.getId(), new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument dc) {
						document.setPages(dc.getPreviewPages());
						pageCursor.setMaxValue((double) document.getPreviewPages());
						pageCursor.setNumValues(document.getPreviewPages());
					}
				});

			showAnnotations(page);

			// Calculate the zoom to see the complete page without scroll
			double zoom = (double) bottom.getHeight() / (double) pageDrawingPane.getImageHeight();
			zoomItem.setValue(zoom);
			pageDrawingPane.zoom(zoom);
		});
		bottom.addMember(pageDrawingPane);
	}

	protected String getPageUrl(int page) {
		return Util.contextPath() + "convertjpg?docId=" + document.getId() + "&fileVersion=" + fileVersion + "&page="
				+ page + "&random=" + new Date().getTime();
	}

	protected void showCurrentPage() {
		showPage((Integer) pageCursor.getValue());
	}

	/**
	 * Factory method to create the proper item for drawing an annotation
	 * 
	 * @param note The annotation to show
	 * @param drawingPane the drawing panel
	 * 
	 * @return the proper item
	 */
	protected DrawItem newAnnotationItem(GUIDocumentNote note) {
		double left = (note.getLeft() * (double) pageDrawingPane.getImageWidth());
		double top = (note.getTop() * (double) pageDrawingPane.getImageHeight());
		double width = (note.getWidth() * (double) pageDrawingPane.getImageWidth());
		double height = (note.getHeight() * (double) pageDrawingPane.getImageHeight());

		final DrawItem drawItem;

		if (note.isSquareShape()) {
			DrawRect square = new DrawRect();
			square.setLeft((int) Math.round(left));
			square.setTop((int) Math.round(top));
			square.setWidth((int) Math.round(width));
			square.setHeight((int) Math.round(height));
			drawItem = square;
		} else if ("circle".equals(note.getShape())) {
			DrawOval circle = new DrawOval();
			circle.setLeft((int) Math.round(left));
			circle.setTop((int) Math.round(top));
			circle.setWidth((int) Math.round(width));
			circle.setHeight((int) Math.round(height));
			drawItem = circle;
		} else if ("line".equals(note.getShape()) || "arrow".equals(note.getShape())) {
			DrawLine line = new DrawLine();
			line.setStartLeft((int) Math.round(left));
			line.setStartTop((int) Math.round(top));
			line.setEndLeft((int) Math.round(width));
			line.setEndTop((int) Math.round(height));
			if ("arrow".equals(note.getShape()))
				line.setEndArrow(ArrowStyle.OPEN);
			drawItem = line;
		} else if ("label".equals(note.getShape())) {
			DrawLabel label = new DrawLabel();
			label.setLeft((int) Math.round(left));
			label.setTop((int) Math.round(top));
			label.setFontSize(note.getLineWidth() > 5 ? note.getLineWidth() : 25);
			label.setFontFamily("Arial, Helvetica, sans-serif");
			label.setContents(Util.strip(note.getMessage()));
			drawItem = label;
		} else if ("thickarrow".equals(note.getShape())) {
			int virtualPixelWidth = (int) Math.round(width / 10);
			int virtualPixelHeight = (int) Math.round(height / 10);

			DrawShape shape = new DrawShape();
			shape.setCommands(new MoveToCommand(new Point(left, top + 3d * virtualPixelHeight)),
					new LineToCommand(new Point(left + 7d * virtualPixelWidth, top + 3d * virtualPixelHeight),
							new Point(left + 7d * virtualPixelWidth, top + 2d * virtualPixelHeight),
							new Point(left + width, top + (height / 2d)),
							new Point(left + 7d * virtualPixelWidth, top + 8d * virtualPixelHeight),
							new Point(left + 7d * virtualPixelWidth, top + 7d * virtualPixelHeight),
							new Point(left, top + 7d * virtualPixelHeight)),
					new CloseCommand());
			drawItem = shape;
		} else if ("comment".equals(note.getShape())) {
			int virtualPixelWidth = (int) Math.round(width / 10);
			int virtualPixelHeight = (int) Math.round(height / 10);

			DrawShape shape = new DrawShape();
			shape.setCommands(new MoveToCommand(new Point(left, top)),
					new LineToCommand(new Point(left + width, top),
							new Point(left + width, top + 8d * virtualPixelHeight),
							new Point(left + 6d * virtualPixelWidth, top + 8d * virtualPixelHeight),
							new Point(left + 4d * virtualPixelWidth, top + height),
							new Point(left + 4d * virtualPixelWidth, top + 8d * virtualPixelHeight),
							new Point(left, top + 8d * virtualPixelHeight), new Point(left, top)),
					new CloseCommand());
			drawItem = shape;
		} else
			return null;

		if (!(drawItem instanceof DrawLine)) {
			drawItem.setFillColor(note.getColor());
			drawItem.setFillOpacity((float) note.getOpacity() / 100.0f);
		}

		drawItem.setCursor(Cursor.HAND);
		drawItem.setCanHover(true);
		drawItem.setLineColor(note.getLineColor());
		drawItem.setLineWidth(note.getLineWidth());
		drawItem.setLineOpacity((float) note.getLineOpacity() / 100.0f);
		drawItem.setShowResizeOutline(false);
		drawItem.setKeepInParentRect(true);
		drawItem.setTitleAutoFit(true);
		drawItem.setTitleRotationMode(TitleRotationMode.NEVER_ROTATE);
		drawItem.setPrompt(note.getMessage());
		drawItem.setRotation(note.getRotation());

		if (!(drawItem instanceof DrawLabel))
			drawItem.setTitle(Util.strip(note.getMessage()));

		if (note.isShowKnobs())
			showKnowbs(drawItem);

		drawItem.addMovedHandler(event -> note.setMovedOrResized(true));
		drawItem.addResizedHandler(event -> note.setMovedOrResized(true));

		return drawItem;
	}

	/**
	 * Shows the knobs to edit an item
	 * 
	 * @param drawItem the item to process
	 */
	public static void showKnowbs(DrawItem drawItem) {
		drawItem.setCanDrag(true);
		if (drawItem instanceof DrawRect || drawItem instanceof DrawShape || drawItem instanceof DrawGroup)
			drawItem.showKnobs(KnobType.RESIZE, KnobType.ROTATE);
		else if (drawItem instanceof DrawOval)
			drawItem.showKnobs(KnobType.RESIZE);
		else if (drawItem instanceof DrawLabel)
			drawItem.showKnobs(KnobType.ROTATE);
		else if (drawItem instanceof DrawLine)
			drawItem.showKnobs(KnobType.STARTPOINT, KnobType.ENDPOINT);
	}

	/**
	 * Hides the knobs to edit an item
	 * 
	 * @param drawItem the item to process
	 */
	public static void hideKnowbs(DrawItem drawItem) {
		drawItem.setCanDrag(false);
		drawItem.hideAllKnobs();
	}
}