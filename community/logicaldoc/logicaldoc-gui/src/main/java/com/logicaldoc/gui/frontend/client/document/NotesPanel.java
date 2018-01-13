package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.annnotation.AnnotationEditor;
import com.logicaldoc.gui.frontend.client.annnotation.AnnotationsWindow;
import com.logicaldoc.gui.frontend.client.services.AnnotationsService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the notes on a document
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.2
 */
public class NotesPanel extends DocumentDetailTab {

	private ListGrid notesGrid;

	private Button addNote;

	private HLayout buttons;

	private VLayout container = new VLayout();

	public NotesPanel(final GUIDocument document) {
		super(document, null);
		addMember(container);
		container.setMembersMargin(2);

		init();
	}

	private void init() {
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

		ListGridField user = new ListGridField("user", I18N.message("author"), 200);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.LEFT);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField page = new ListGridField("page", I18N.message("page"), 80);
		page.setWidth("*");

		notesGrid = new ListGrid() {
			@Override
			protected Canvas getExpansionComponent(final ListGridRecord record) {
				return new HTMLFlow(
						"<div class='details'>"
								+ (record.getAttributeAsString("message") != null ? record
										.getAttributeAsString("message") : "") + "</div>");
			}
		};
		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, document.getId(), null));
		if (Feature.enabled(Feature.ANNOTATIONS))
			notesGrid.setFields(id, userId, user, date, page);
		else
			notesGrid.setFields(id, userId, user, date);
		notesGrid.setWidth100();
		notesGrid.setCanExpandRecords(true);
		notesGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
		notesGrid.setDetailField("message");

		container.setHeight100();
		container.setWidth100();
		container.addMember(notesGrid);

		addNote = new Button(I18N.message("addnote"));
		addNote.setAutoFit(true);
		addNote.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AnnotationEditor note = new AnnotationEditor(document.getId(), null, NotesPanel.this, null, "", null, 0);
				note.show();
			}
		});

		Button annotations = new Button(I18N.message("annotations"));
		annotations.setAutoFit(true);
		annotations.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				AnnotationsService.Instance.get().prepareAnnotations(document.getId(), null,
						new AsyncCallback<Integer>() {
							@Override
							public void onFailure(Throwable caught) {
								ContactingServer.get().hide();
								SC.warn(I18N.message("unabletoprepareforannotations"));
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Integer pages) {
								ContactingServer.get().hide();
								if (pages == null || pages.intValue() < 1) {
									SC.warn(I18N.message("unabletoprepareforannotations"));
								} else {
									AnnotationsWindow dialog = new AnnotationsWindow(document.getId(), document
											.getFileName(), pages);
									dialog.show();
								}
							}
						});
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
						AnnotationEditor note = new AnnotationEditor(document.getId(), notesGrid.getSelectedRecord()
								.getAttributeAsLong("id"), NotesPanel.this, null, notesGrid.getSelectedRecord()
								.getAttribute("message"), null, 0);
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

		// Expand all notes after arrived
		notesGrid.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				for (ListGridRecord rec : notesGrid.getRecords()) {
					notesGrid.expandRecord(rec);
				}
			}
		});
	}

	private void onDelete() {
		ListGridRecord[] selection = notesGrid.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++) {
			if ("0".equals(selection[i].getAttribute("page")))
				ids[i] = Long.parseLong(selection[i].getAttribute("id"));
		}

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
			@Override
			public void execute(Boolean value) {
				if (value) {
					DocumentService.Instance.get().deleteNotes(ids, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
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

	public void onAdded(long id, String message) {
		init();
	}

	public void onUpdated(String message) {
		ListGridRecord record = notesGrid.getSelectedRecord();
		record.setAttribute("username", Session.get().getUser().getFullName());
		record.setAttribute("date", new Date());
		record.setAttribute("message", message);
		notesGrid.refreshRow(notesGrid.getRecordIndex(record));
		notesGrid.collapseRecord(record);
		notesGrid.expandRecord(record);
	}
}