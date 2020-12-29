package com.logicaldoc.gui.frontend.client.document.note;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A superclass for those windows that display annotations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public abstract class AbstractAnnotationsWindow extends Window {

	protected SpinnerItem pageCursor;

	protected HLayout bottom = new HLayout();

	protected GUIDocument document;

	protected ImageWithCanvases pageImage;

	protected List<GUIDocumentNote> notes = new ArrayList<GUIDocumentNote>();

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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocumentNote[] nts) {
						notes.addAll(Arrays.asList(nts));
						initGUI();
					}
				});
	}

	protected void showAnnotations(int page) {
		for (GUIDocumentNote note : notes)
			if (note.getPage() == page) {
				Canvas noteCanvas = getAnnotationCanvas(note);
				pageImage.addCanvas(noteCanvas);
			}
	}

	/**
	 * Concrete implementations should provide a Canvas representing the passed
	 * note
	 * 
	 * @param note the note to use to create the Canvas
	 * 
	 * @return The Canvas displaying the note
	 */
	protected abstract Canvas getAnnotationCanvas(GUIDocumentNote note);

	/**
	 * Saves the actual positions of the notes in the current page
	 */
	protected void captureNotesPosition() {
		if (pageImage != null && pageImage.getCanvases() != null) {
			List<Canvas> canvases = pageImage.getCanvases();
			for (Canvas canvas : canvases) {
				AnnotationCanvas noteCanvas = (AnnotationCanvas) canvas;
				noteCanvas.captureNotePosition();
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
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						onNotesSaved();
					}
				});
	}

	public void deleteNote(GUIDocumentNote note) {
		notes.remove(note);
		showCurrentPage();
	}

	protected void initGUI() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		ToolStripButton zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (pageImage != null) {
					captureNotesPosition();
					pageImage.clearCanvases();
					pageImage.resize(100);
					showAnnotations((Integer) pageCursor.getValue());
				}
			}
		});

		ToolStripButton zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (pageImage != null) {
					captureNotesPosition();
					pageImage.clearCanvases();
					pageImage.resize(-100);
					showAnnotations((Integer) pageCursor.getValue());
				}
			}
		});

		pageCursor = ItemFactory.newSpinnerItem("page", "page", 1, 1,
				document.getPages() > 0 ? document.getPages() : 1);
		pageCursor.setHint("/" + (document.getPages() > 0 ? document.getPages() : 1));
		pageCursor.setSaveOnEnter(true);
		pageCursor.setImplicitSave(true);
		pageCursor.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				showCurrentPage();
			}
		});

		toolStrip.addFormItem(pageCursor);
		toolStrip.addButton(zoomIn);
		toolStrip.addButton(zoomOut);
		toolStrip.addSeparator();

		prepareAdditionalActions(toolStrip);

		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(bottom);

		showPage(1);
	}

	/**
	 * Invoked to populate the toolbar with additional actions
	 * 
	 * @param toolStrip The toolstrip to update
	 */
	protected abstract void prepareAdditionalActions(ToolStrip toolStrip);

	public ImageWithCanvases getPageImage() {
		return pageImage;
	}

	/**
	 * Invoked when the notes have been saved
	 */
	protected abstract void onNotesSaved();

	void showPage(int page) {
		if (pageImage != null) {
			captureNotesPosition();
			bottom.removeMembers(bottom.getMembers());
		}

		pageImage = new ImageWithCanvases(getPageUrl(page), null, new CallBack() {

			@Override
			public void onImagesLoaded(ImageElement[] imageElements) {
				// Reload the document to update the pages count
				if (document.getPages() <= 1)
					DocumentService.Instance.get().getById(document.getId(), new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument dc) {
							document.setPages(dc.getPages());
							pageCursor.setMax(document.getPages());
							pageCursor.setHint("/" + (document.getPages() > 0 ? document.getPages() : 1));
						}
					});
				showAnnotations(page);
			}
		});
		bottom.addMember(pageImage);
	}

	private String getPageUrl(int page) {
		return Util.contextPath() + "convertjpg?docId=" + document.getId() + "&fileVersion=" + fileVersion + "&page="
				+ page + "&random=" + new Date().getTime();
	}

	protected void showCurrentPage() {
		showPage((Integer) pageCursor.getValue());
	}
}