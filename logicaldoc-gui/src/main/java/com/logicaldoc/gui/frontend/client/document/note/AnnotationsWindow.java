package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.NoteChangeListener;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import com.smartgwt.client.widgets.toolbar.ToolStripMenuButton;

/**
 * A Window to display annotations in a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class AnnotationsWindow extends AbstractAnnotationsWindow {

	private static final String COMMENT_ALT = "comment-alt";

	private static final String SQUARE2 = "square";

	private static final String LABEL = "label";

	private static final String CIRCLE = "circle";

	private static final String COMMENT = "comment";

	private static final String COMMENT_FLIPPED = "comment-flipped";

	private boolean editEnabled = true;

	private NoteChangeListener saveHandler;

	/**
	 * Constructor
	 * 
	 * @param doc the document to be displayed
	 * @param fileVer file version specification, null to use the current
	 *        version
	 * @param saveHandler the handler to invoke on save
	 * @param editEnabled if the user can edit the annotations
	 */
	public AnnotationsWindow(GUIDocument doc, String fileVer, NoteChangeListener saveHandler, boolean editEnabled) {
		super(doc, fileVer != null ? fileVer : doc.getFileVersion(), null);

		this.saveHandler = saveHandler;
		this.editEnabled = editEnabled;
	}

	@Override
	protected DrawItem prepareAnnotationItem(GUIDocumentNote note) {
		DrawItem drawItem = newAnnotationItem(note);
		if (editEnabled && drawItem != null) {
			AnnotationContextMenu contextMenu = new AnnotationContextMenu(drawItem, note);
			contextMenu.addDeleteClickHandler(
					event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
						if (Boolean.TRUE.equals(confirm)) {
							notes.remove(note);
							drawItem.erase();
						}
					}));
			drawItem.setContextMenu(contextMenu);
		}
		return drawItem;
	}

	private void addNewNote(String shape) {
		GUIDocumentNote newNote = new GUIDocumentNote();
		newNote.setDocId(document.getId());
		newNote.setFileVersion(fileVersion);
		newNote.setPage((Integer) pageCursor.getValue());
		newNote.setShowKnobs(true);
		newNote.setMessage("new note");
		newNote.setShape(shape);

		if (newNote.getShape().equals(COMMENT) || newNote.getShape().equals(COMMENT_FLIPPED)) {
			newNote.setHeight(0.06);
			newNote.setShape(COMMENT);
			if (shape.equals(COMMENT_FLIPPED))
				newNote.setRotation(180.0);
		} else if (newNote.getShape().equals(CIRCLE)) {
			newNote.setHeight(0.10);
		} else if (newNote.getShape().equals(LABEL)) {
			newNote.setLineWidth(25);
			newNote.setLineColor("red");
		} else if (newNote.getShape().equals("line") || newNote.getShape().equals("arrow")) {
			newNote.setLineColor("red");
			newNote.setLineWidth(3);
		}
		notes.add(newNote);

		showCurrentPage();
	}

	@Override
	protected void prepareAdditionalActions(ToolStrip toolStrip) {
		if (editEnabled) {
			ToolStripButton addLabel = AwesomeFactory.newToolStripButton("font", LABEL);
			addLabel.addClickHandler((ClickEvent event) -> addNewNote(LABEL));

			toolStrip.addMenuButton(getPolygonMenuButton());
			toolStrip.addMenuButton(getLineMenuButton());
			toolStrip.addMenuButton(getShapeMenuButton());
			toolStrip.addButton(addLabel);
			toolStrip.addSeparator();

			ToolStripButton save = new ToolStripButton();
			save.setTitle(I18N.message("save"));
			save.addClickHandler((ClickEvent event) -> onSave());
			toolStrip.addButton(save);

		}
	}

	private ToolStripMenuButton getShapeMenuButton() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem arrow = new MenuItem(AwesomeFactory.getIconHtml("arrow-alt-right"));
		arrow.addClickHandler(click -> addNewNote("thickarrow"));

		MenuItem comment = new MenuItem(AwesomeFactory.getIconHtml(COMMENT_ALT));
		comment.addClickHandler(click -> addNewNote(COMMENT));

		MenuItem commentFlipped = new MenuItem(AwesomeFactory.getIconHtml(COMMENT_ALT, "fa-rotate-180", null));
		commentFlipped.addClickHandler(click -> addNewNote(COMMENT_FLIPPED));

		menu.setData(comment, commentFlipped, arrow);

		return AwesomeFactory.newToolStripToolStripMenuButton(COMMENT_ALT, null, null, "shape", menu);
	}

	private ToolStripMenuButton getPolygonMenuButton() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem square = new MenuItem(AwesomeFactory.getIconHtml(SQUARE2));
		square.addClickHandler(click -> addNewNote(SQUARE2));

		MenuItem circle = new MenuItem(AwesomeFactory.getIconHtml(CIRCLE));
		circle.addClickHandler(click -> addNewNote(CIRCLE));

		menu.setItems(square, circle);

		return AwesomeFactory.newToolStripToolStripMenuButton(SQUARE2, null, null, "polygon", menu);
	}

	private ToolStripMenuButton getLineMenuButton() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem line = new MenuItem(AwesomeFactory.getIconHtml("horizontal-rule"));
		line.addClickHandler(click -> addNewNote("line"));

		MenuItem arrow = new MenuItem(AwesomeFactory.getIconHtml("long-arrow-right"));
		arrow.addClickHandler(click -> addNewNote("arrow"));

		menu.setItems(line, arrow);

		return AwesomeFactory.newToolStripToolStripMenuButton("horizontal-rule", null, null, "line", menu);
	}

	@Override
	protected void onNotesSaved() {
		if (saveHandler != null)
			saveHandler.onChanged();
		destroy();
	}
}