package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to add a note in a visually defined position
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class AnnotationsWindow extends AbstractAnnotationsWindow {

	private NotesPanel notesPanel;

	private boolean writeEnabled = true;

	/**
	 * Constructor
	 * 
	 * @param doc the document to be displayed
	 * @param fileVer file version specification, null to use the current
	 *        version
	 * @param notesPanel the notes panel associated to this window, optional
	 * @param writeEnabled if the user can edit the annotations
	 */
	public AnnotationsWindow(GUIDocument doc, String fileVer, NotesPanel notesPanel, boolean writeEnabled) {
		super(doc, fileVer, null);

		this.notesPanel = notesPanel;
		this.writeEnabled = writeEnabled;
	}

	@Override
	protected Canvas getAnnotationCanvas(GUIDocumentNote note) {
		return new AnnotationCanvas(note, this, writeEnabled);
	}

	@Override
	protected void onNotesSaved() {
		if (notesPanel != null)
			notesPanel.refresh();
		destroy();
	}

	@Override
	protected void prepareAdditionalActions(ToolStrip toolStrip) {
		if (!writeEnabled)
			return;

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addannotation"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				captureNotesPosition();

				GUIDocumentNote note = new GUIDocumentNote();
				note.setDocId(document.getId());
				note.setUserId(Session.get().getUser().getId());
				note.setUsername(Session.get().getUser().getFullName());
				note.setMessage(I18N.message("annotation").toLowerCase() + "...");
				note.setPage((Integer) pageCursor.getValue());
				notes.add(note);

				Canvas noteCanvas = new AnnotationCanvas(note, AnnotationsWindow.this, writeEnabled);
				pageImage.addCanvas(noteCanvas);
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
	}
}