package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
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
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
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

		ListGridField id = new IdListGridField();

		UserListGridField user = new UserListGridField("user", USER_ID, "author");
		ListGridField date = new DateListGridField("date", "date");

		ListGridField page = new ListGridField("page", I18N.message("page"), 50);
		page.setAutoFitWidth(true);
		page.setAlign(Alignment.CENTER);

		ListGridField fileVersion = new ListGridField("fileVersion", I18N.message("fileversion"), 50);
		fileVersion.setHidden(true);

		ListGridField content = new ListGridField(MESSAGE, I18N.message("content"), 70);
		content.setWidth("*");

		notesGrid = new ListGrid();
		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, document.getId(), document.getFileVersion(), null));
		notesGrid.setFields(id, user, date, page, fileVersion, content);

		toolStrip = new ToolStrip();
		toolStrip.setWidth100();

		ToolStripButton addNote = new ToolStripButton(I18N.message("addnote"));
		addNote.addClickHandler(
				click -> new NoteUpdateDialog(new GUIDocumentNote(document), nt -> refresh()).show());

		ToolStripButton annotations = new ToolStripButton(I18N.message("annotations"));
		annotations.addClickHandler(
				click -> new com.logicaldoc.gui.frontend.client.document.note.AnnotationsWindow(document, null,
						nt -> refresh(), true).show());

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		export.addClickHandler(click -> GridUtil.exportCSV(notesGrid, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(click -> GridUtil.print(notesGrid));

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

		notesGrid.addCellContextClickHandler(click -> {
			click.cancel();
			DocumentService.Instance.get().getNote(notesGrid.getSelectedRecord().getAttributeAsLong("id"),
					new DefaultAsyncCallback<GUIDocumentNote>() {
						@Override
						protected void handleSuccess(GUIDocumentNote note) {
							Menu contextMenu = new Menu();
							MenuItem delete = new MenuItem();
							delete.setTitle(I18N.message("ddelete"));
							delete.setEnabled(false);
							delete.addClickHandler(clickEvent -> onDelete());

							MenuItem edit = new MenuItem();
							edit.setTitle(I18N.message("edit"));
							edit.setEnabled(false);
							edit.addClickHandler(click -> new NoteUpdateDialog(note, snt -> refresh()).show());

							MenuItem security = new MenuItem();
							security.setTitle(I18N.message("security"));
							security.setEnabled(false);
							security.addClickHandler(
									click -> new NoteSecurityDialog(note, NotesPanel.this::onSecurityChanged).show());

							MenuItem prnt = new MenuItem();
							prnt.setTitle(I18N.message("print"));
							prnt.addClickHandler(click -> {
								HTMLPane printContainer = new HTMLPane();
								printContainer.setContents(notesGrid.getSelectedRecord().getAttribute(MESSAGE));
								Canvas.showPrintPreview(printContainer);
							});

							ListGridRecord[] selection = notesGrid.getSelectedRecords();

							boolean editingFlag = Session.get().getConfigAsBoolean("gui.notes.allowedit");
							edit.setEnabled(selection.length == 1 && editingFlag && note.isWrite());
							security.setEnabled(selection.length == 1 && note.isSecurity());
							delete.setEnabled((selection.length == 1 && editingFlag && note.isDelete())
									|| Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN));
							prnt.setEnabled(selection.length == 1);

							contextMenu.setItems(edit, security, prnt, new MenuItemSeparator(), delete);
							contextMenu.showContextMenu();
						}
					});
		});
	}

	private void onDelete() {
		ListGridRecord[] selection = notesGrid.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				DocumentService.Instance.get().deleteNotes(GridUtil.getIds(selection), new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(Void result) {
						notesGrid.removeSelectedData();
						DocumentService.Instance.get().getById(document.getId(),
								new DefaultAsyncCallback<GUIDocument>() {

									@Override
									public void handleSuccess(GUIDocument result) {
										DocumentController.get().modified(result);
									}
								});
					}
				});
			}
		});
	}

	private void onSecurityChanged(GUIDocumentNote note) {
		DocumentService.Instance.get().saveNote(note, new DefaultAsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				super.onFailure(caught);
				destroy();
			}

			@Override
			public void handleSuccess(GUIDocumentNote result) {
				refresh();
				DocumentService.Instance.get().getById(result.getDocId(), new DefaultAsyncCallback<GUIDocument>() {
					@Override
					protected void handleSuccess(GUIDocument result) {
						DocumentController.get().modified(result);
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