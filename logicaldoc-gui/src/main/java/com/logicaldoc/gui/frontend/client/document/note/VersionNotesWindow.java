package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This window shows the notes of a version
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class VersionNotesWindow extends Window {

	private ListGrid notesGrid;

	private GUIDocument document;

	private String fileVersion;

	public VersionNotesWindow(GUIDocument document, String fileVer) {
		super();

		fileVersion = fileVer;
		if (fileVersion == null)
			fileVersion = document.getFileVersion();

		this.document = document;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("notes") + " - " + document.getFileName() + " v" + fileVersion);
		setWidth100();
		setHeight100();

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		ToolStripButton annotations = new ToolStripButton();
		annotations.setTitle(I18N.message("annotations"));
		annotations.addClickHandler(event -> new AnnotationsWindow(document,
				fileVer != null ? fileVer : document.getFileVersion(), null, false).show());

		ToolStripButton addNote = new ToolStripButton(I18N.message("addnote"));
		addNote.addClickHandler(
				event -> new NoteUpdateDialog(document.getId(), 0L, fileVersion, null, save -> refresh()).show());

		toolStrip.addButton(annotations);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);
		refresh();
	}

	protected void refresh() {
		if (notesGrid != null)
			removeItem(notesGrid);

		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);

		ListGridField fileVersionField = new ListGridField("fileVersion", I18N.message("fileversion"), 50);
		fileVersionField.setHidden(true);

		UserListGridField user = new UserListGridField("user", "userId", "author");
		DateListGridField date = new DateListGridField("date", "date");
		ListGridField page = new ListGridField("page", I18N.message("page"), 50);
		page.setAutoFitWidth(true);
		page.setAlign(Alignment.CENTER);

		ListGridField content = new ListGridField("message", I18N.message("content"), 70);
		content.setWidth("*");

		ListGrid notesGrid = new ListGrid();

		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, document.getId(), fileVersion, null));
		notesGrid.setFields(id, user, date, page, fileVersionField, content);
		notesGrid.setWidth100();

		addItem(notesGrid);
	}
}