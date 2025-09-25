package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This window shows the notes of a version
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class VersionNotesWindow extends Window {

	private static final String MESSAGE = "message";

	private GUIDocument document;

	private String fileVersion;

	private RefreshableListGrid notesGrid = new RefreshableListGrid();

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
		annotations.addClickHandler(
				event -> new AnnotationsWindow(document, fileVersion, this::refresh, document.getFolder().isWrite())
						.show());

		ToolStripButton addNote = new ToolStripButton(I18N.message("addnote"));
		addNote.addClickHandler(
				event -> new NoteUpdateDialog(document.getId(), 0L, fileVersion, null, this::refresh).show());

		if (document.getFolder().isWrite())
			toolStrip.addButton(addNote);
		toolStrip.addButton(annotations);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		ListGridField id = new IdListGridField();

		ListGridField fileVersionField = new ListGridField("fileVersion", I18N.message("fileversion"), 50);
		fileVersionField.setHidden(true);

		UserListGridField user = new UserListGridField("user", "userId", "author");
		DateListGridField date = new DateListGridField("date", "date");
		ListGridField page = new ListGridField("page", I18N.message("page"), 50);
		page.setAutoFitWidth(true);
		page.setAlign(Alignment.CENTER);

		ListGridField content = new ListGridField(MESSAGE, I18N.message("content"), 70);
		content.setWidth("*");

		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, document.getId(), fileVersion, null));
		notesGrid.setFields(id, user, date, page, fileVersionField, content);
		notesGrid.setWidth100();
		notesGrid.addCellContextClickHandler(event -> {
			Menu contextMenu = new Menu();

			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.setEnabled(false);
			delete.addClickHandler(clickEvent -> onDelete());

			MenuItem edit = new MenuItem();
			edit.setTitle(I18N.message("edit"));
			edit.setEnabled(false);
			edit.addClickHandler(click -> new NoteUpdateDialog(document.getId(),
					notesGrid.getSelectedRecord().getAttributeAsLong("id"), null,
					notesGrid.getSelectedRecord().getAttribute(MESSAGE), this::refresh).show());

			MenuItem prnt = new MenuItem();
			prnt.setTitle(I18N.message("print"));
			prnt.addClickHandler(clickEvent -> {
				HTMLPane printContainer = new HTMLPane();
				printContainer.setContents(notesGrid.getSelectedRecord().getAttribute(MESSAGE));
				Canvas.showPrintPreview(printContainer);
			});

			ListGridRecord[] selection = notesGrid.getSelectedRecords();

			if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
				delete.setEnabled(selection.length > 0);
				edit.setEnabled(selection.length == 1);
			} else if (Session.get().getConfigAsBoolean("gui.notes.allowedit")) {
				long usrId = Long.parseLong(selection[0].getAttribute("userId"));
				delete.setEnabled(selection.length == 1 && usrId == Session.get().getUser().getId());
				edit.setEnabled(selection.length == 1 && usrId == Session.get().getUser().getId());
			}

			prnt.setEnabled(selection.length == 1);

			contextMenu.setItems(edit, prnt, new MenuItemSeparator(), delete);
			contextMenu.showContextMenu();
			event.cancel();
		});

		addItem(toolStrip);
		addItem(notesGrid);
	}

	protected void refresh() {
		notesGrid.refresh(new NotesDS(null, document.getId(), fileVersion, null));
	}

	private void onDelete() {
		ListGridRecord[] selection = notesGrid.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				DocumentService.Instance.get().deleteNotes(GridUtil.getIds(selection), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						notesGrid.removeSelectedData();
					}
				});
			}
		});
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}