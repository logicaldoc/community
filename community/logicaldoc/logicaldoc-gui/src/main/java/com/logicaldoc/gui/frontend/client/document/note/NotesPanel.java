package com.logicaldoc.gui.frontend.client.document.note;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentDetailTab;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the notes on a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class NotesPanel extends DocumentDetailTab {

	private ListGrid notesGrid;

	private IButton addNote;

	private HLayout buttons;

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
		if (addNote != null)
			container.removeMember(addNote);
		if (notesGrid != null)
			container.removeMember(notesGrid);
		if (buttons != null)
			container.removeMember(buttons);

		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);

		ListGridField userId = new ListGridField("userId", "userid", 50);
		userId.setHidden(true);

		UserListGridField user = new UserListGridField("user", "userId", "author");
		ListGridField date = new DateListGridField("date", "date");

		ListGridField page = new ListGridField("page", I18N.message("page"), 50);
		page.setAutoFitWidth(true);
		page.setAlign(Alignment.CENTER);

		ListGridField content = new ListGridField("message", I18N.message("content"), 70);
		content.setWidth("*");

		notesGrid = new ListGrid();
		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, document.getId(), document.getFileVersion(), null));
		notesGrid.setFields(id, userId, user, date, page, content);
		container.setHeight100();
		container.setWidth100();
		container.addMember(notesGrid);

		addNote = new IButton(I18N.message("addnote"));
		addNote.setAutoFit(false);
		addNote.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				NoteUpdateDialog note = new NoteUpdateDialog(document.getId(), 0L, null, NotesPanel.this);
				note.show();
			}
		});

		IButton annotations = new IButton(I18N.message("annotations"));
		annotations.setAutoFit(false);
		annotations.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AnnotationsWindow dialog = new com.logicaldoc.gui.frontend.client.document.note.AnnotationsWindow(
						document, null, NotesPanel.this, true);
				dialog.show();
			}
		});

		buttons = new HLayout();
		buttons.setWidth100();
		buttons.setHeight(30);
		buttons.setMembersMargin(5);
		container.addMember(buttons);

		if (document.getFolder().isWrite()) {
			buttons.addMember(addNote);
		}

		if (Feature.visible(Feature.ANNOTATIONS)) {
			buttons.addMember(annotations);
			annotations.setDisabled(!Feature.enabled(Feature.ANNOTATIONS));
		}

		notesGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = new Menu();
				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						onDelete();
					}
				});

				MenuItem edit = new MenuItem();
				edit.setTitle(I18N.message("edit"));
				edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						NoteUpdateDialog note = new NoteUpdateDialog(document.getId(),
								notesGrid.getSelectedRecord().getAttributeAsLong("id"),
								notesGrid.getSelectedRecord().getAttribute("message"), NotesPanel.this);
						note.show();
					}
				});

				MenuItem print = new MenuItem();
				print.setTitle(I18N.message("print"));
				print.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						HTMLPane printContainer = new HTMLPane();
						printContainer.setContents(notesGrid.getSelectedRecord().getAttribute("message"));
						Canvas.showPrintPreview(printContainer);
					}
				});

				ListGridRecord[] selection = notesGrid.getSelectedRecords();

				if (Session.get().getUser().isMemberOf("admin")) {
					delete.setEnabled(selection.length > 0);
					edit.setEnabled(selection.length == 1);
				} else {
					long userId = Long.parseLong(selection[0].getAttribute("userId"));
					delete.setEnabled(selection.length == 1 && userId == Session.get().getUser().getId());
					edit.setEnabled(selection.length == 1 && userId == Session.get().getUser().getId());
				}

				print.setEnabled(selection.length == 1);

				contextMenu.setItems(edit, print, delete);
				contextMenu.showContextMenu();
				event.cancel();
			}
		});
	}

	private void onDelete() {
		ListGridRecord[] selection = notesGrid.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++)
			ids[i] = selection[i].getAttributeAsLong("id");

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
			@Override
			public void execute(Boolean value) {
				if (value) {
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
			}
		});
	}
}