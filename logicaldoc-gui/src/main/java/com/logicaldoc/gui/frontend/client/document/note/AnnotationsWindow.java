package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
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

	private NotesPanel notesPanel;

	private boolean editEnabled = true;

	/**
	 * Constructor
	 * 
	 * @param doc the document to be displayed
	 * @param fileVer file version specification, null to use the current
	 *        version
	 * @param notesPanel the notes panel associated to this window, optional
	 * @param editEnabled if the user can edit the annotations
	 */
	public AnnotationsWindow(GUIDocument doc, String fileVer, NotesPanel notesPanel, boolean editEnabled) {
		super(doc, fileVer != null ? fileVer : doc.getFileVersion(), null);

		this.notesPanel = notesPanel;
		this.editEnabled = editEnabled;
	}

	@Override
	protected DrawItem prepareAnnotationItem(GUIDocumentNote note) {
		DrawItem drawItem = newAnnotationItem(note);
		if (editEnabled && drawItem != null) {
			AnnotationContextMenu contextMenu = new AnnotationContextMenu(drawItem, note);
			contextMenu.addDeleteClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

				@Override
				public void onClick(com.smartgwt.client.widgets.menu.events.MenuItemClickEvent event) {
					LD.ask(I18N.message("question"), I18N.message("confirmdelete"),(Boolean value) -> {
						if (Boolean.TRUE.equals(value)) {
								notes.remove(note);
								drawItem.erase();
							}
					});
				}
			});
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

		if (newNote.getShape().equals("comment") || newNote.getShape().equals("comment-flipped")) {
			newNote.setHeight(0.06);
			newNote.setShape("comment");
			if (shape.equals("comment-flipped"))
				newNote.setRotation(180.0);
		} else if (newNote.getShape().equals("circle")) {
			newNote.setHeight(0.10);
		} else if (newNote.getShape().equals("label")) {
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
			ToolStripButton addLabel = AwesomeFactory.newToolStripButton("font", "label");
			addLabel.addClickHandler(new ClickHandler() {

				@Override
				public void onClick(ClickEvent event) {
					addNewNote("label");
				}
			});

			toolStrip.addMenuButton(getPolygonMenuButton());
			toolStrip.addMenuButton(getLineMenuButton());
			toolStrip.addMenuButton(getShapeMenuButton());
			toolStrip.addButton(addLabel);
			toolStrip.addSeparator();

			ToolStripButton save = new ToolStripButton();
			save.setTitle(I18N.message("save"));
			save.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					onSave();
				}
			});
			toolStrip.addButton(save);

		}
	}

	private ToolStripMenuButton getShapeMenuButton() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem arrow = new MenuItem(AwesomeFactory.getIconHtml("arrow-alt-right"));
		arrow.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("thickarrow");
			}
		});

		MenuItem comment = new MenuItem(AwesomeFactory.getIconHtml("comment-alt"));
		comment.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("comment");
			}
		});

		MenuItem commentFlipped = new MenuItem(AwesomeFactory.getIconHtml("comment-alt", "fa-rotate-180", null));
		commentFlipped.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("comment-flipped");
			}
		});

		menu.setData(comment, commentFlipped, arrow);

		return AwesomeFactory.newToolStripToolStripMenuButton("comment-alt", null, null, "shape", menu);
	}

	private ToolStripMenuButton getPolygonMenuButton() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem square = new MenuItem(AwesomeFactory.getIconHtml("square"));
		square.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("square");
			}
		});

		MenuItem circle = new MenuItem(AwesomeFactory.getIconHtml("circle"));
		circle.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("circle");
			}
		});

		menu.setItems(square, circle);

		return AwesomeFactory.newToolStripToolStripMenuButton("square", null, null, "polygon", menu);
	}

	private ToolStripMenuButton getLineMenuButton() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem line = new MenuItem(AwesomeFactory.getIconHtml("horizontal-rule"));
		line.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("line");
			}
		});

		MenuItem arrow = new MenuItem(AwesomeFactory.getIconHtml("long-arrow-right"));
		arrow.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

			@Override
			public void onClick(MenuItemClickEvent event) {
				addNewNote("arrow");
			}
		});

		menu.setItems(line, arrow);

		return AwesomeFactory.newToolStripToolStripMenuButton("horizontal-rule", null, null, "line", menu);
	}

	@Override
	protected void onNotesSaved() {
		if (notesPanel != null)
			notesPanel.refresh();
		destroy();
	}
}