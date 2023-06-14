package com.logicaldoc.gui.frontend.client.document.note;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentDetailTab;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the notes on a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class NotesPanel extends DocumentDetailTab {

	private static final String MESSAGE = "message";

	private static final String USER_ID = "userId";

	private ListGrid notesGrid;

	private ToolStrip toolStrip;
	
	private VLayout container = new VLayout();

	public NotesPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		addMember(container);
		container.setMembersMargin(2);
		refresh();
	}

	public void refresh() {
		if (notesGrid != null)
			container.removeMember(notesGrid);
		
		if (toolStrip != null)
			container.removeMember(toolStrip);

		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);

		ListGridField userId = new ListGridField(USER_ID, "userid", 50);
		userId.setHidden(true);

		UserListGridField user = new UserListGridField("user", USER_ID, "author");
		ListGridField date = new DateListGridField("date", "date");

		ListGridField page = new ListGridField("page", I18N.message("page"), 50);
		page.setAutoFitWidth(true);
		page.setAlign(Alignment.CENTER);

		ListGridField content = new ListGridField(MESSAGE, I18N.message("content"), 70);
		content.setWidth("*");

		notesGrid = new ListGrid();
		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, document.getId(), document.getFileVersion(), null));
		notesGrid.setFields(id, userId, user, date, page, content);

		toolStrip = new ToolStrip();
		toolStrip.setWidth100();

		ToolStripButton addNote = new ToolStripButton(I18N.message("addnote"));
		addNote.addClickHandler(event -> new NoteUpdateDialog(document.getId(), 0L, null, NotesPanel.this).show());

		ToolStripButton annotations = new ToolStripButton(I18N.message("annotations"));
		annotations.addClickHandler(
				event -> new com.logicaldoc.gui.frontend.client.document.note.AnnotationsWindow(document, null,
						NotesPanel.this, true).show());

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		export.addClickHandler(event -> GridUtil.exportCSV(notesGrid, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(event -> GridUtil.print(notesGrid));

		if (document.getFolder().isWrite()) {
			toolStrip.addButton(addNote);
		}

		if (Feature.visible(Feature.ANNOTATIONS)) {
			toolStrip.addButton(annotations);
			annotations.setDisabled(!Feature.enabled(Feature.ANNOTATIONS));
		}

		if (document.getFolder().isWrite())
			toolStrip.addSeparator();
		toolStrip.addButton(export);
		toolStrip.addButton(print);

		container.setHeight100();
		container.setWidth100();
		container.addMember(notesGrid);
		container.addMember(toolStrip);

		notesGrid.addCellContextClickHandler(event -> {
			Menu contextMenu = new Menu();
			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.setEnabled(false);
			delete.addClickHandler(clickEvent -> onDelete());

			MenuItem edit = new MenuItem();
			edit.setTitle(I18N.message("edit"));
			edit.setEnabled(false);
			edit.addClickHandler(clickEvent -> {
				NoteUpdateDialog note = new NoteUpdateDialog(document.getId(),
						notesGrid.getSelectedRecord().getAttributeAsLong("id"),
						notesGrid.getSelectedRecord().getAttribute(MESSAGE), NotesPanel.this);
				note.show();
			});

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
				long usrId = Long.parseLong(selection[0].getAttribute(USER_ID));
				delete.setEnabled(selection.length == 1 && usrId == Session.get().getUser().getId());
				edit.setEnabled(selection.length == 1 && usrId == Session.get().getUser().getId());
			}

			prnt.setEnabled(selection.length == 1);

			contextMenu.setItems(edit, prnt, delete);
			contextMenu.showContextMenu();
			event.cancel();
		});
	}

	private void onDelete() {
		ListGridRecord[] selection = notesGrid.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++)
			ids[i] = selection[i].getAttributeAsLong("id");

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				DocumentService.Instance.get().deleteNotes(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						notesGrid.removeSelectedData();
					}
				});
			}
		});
	}
}