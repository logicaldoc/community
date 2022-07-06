package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.AutomationTriggersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
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
 * This panel shows the automation triggers of a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class FolderAutomationPanel extends FolderDetailTab {

	private ListGrid list;

	private VLayout container = new VLayout();

	public FolderAutomationPanel(final GUIFolder folder) {
		super(folder, null);
	}

	@Override
	protected void onDraw() {
		container.setMembersMargin(3);
		addMember(container);
		refresh(folder);
	}

	private void refreshList() {
		if (list != null)
			container.removeMember(list);

		ListGridField id = new ListGridField("id", "id", 50);
		id.setCanEdit(false);
		id.setHidden(true);

		ListGridField automation = new ListGridField("automation", I18N.message("automation"));
		automation.setWidth("*");
		automation.setCanEdit(false);

		ListGridField routine = new ListGridField("routine", I18N.message("routine"), 150);
		routine.setCanFilter(true);

		ListGridField events = new ListGridField("events", I18N.message("triggeron"));
		events.setWidth(300);
		events.setCanEdit(false);
		events.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				try {
					if (value != null && !value.toString().trim().isEmpty()) {
						// Translate the set of events
						String[] key = null;

						if (!value.toString().contains(","))
							key = new String[] { value.toString().trim() };
						else
							key = value.toString().split(",");
						List<String> labels = new ArrayList<String>();
						for (String string : key) {
							if (string.trim().isEmpty())
								continue;
							labels.add(I18N.message(string + ".short"));
						}
						String str = labels.toString().substring(1);
						return str.substring(0, str.length() - 1);
					} else
						return "";
				} catch (Throwable e) {
					return "";
				}
			}
		});

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setDataSource(new AutomationTriggersDS(folder.getId(), null));
		list.setFields(id, events, routine, automation);
		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});
		list.addDoubleClickHandler(new DoubleClickHandler() {

			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				onEdit();
			}
		});

		container.addMember(list, 0);
	}

	void refresh(final GUIFolder folder) {
		super.folder = folder;

		container.removeMembers(container.getMembers());

		refreshList();

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		Button add = new Button(I18N.message("addautomationtrigger"));
		add.setAutoFit(true);
		add.setMargin(1);
		add.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				GUIAutomationTrigger trigger = new GUIAutomationTrigger();
				trigger.setFolder(folder);
				AutomationTriggerDialog dialog = new AutomationTriggerDialog(trigger, FolderAutomationPanel.this);
				dialog.show();
			}
		});

		Button applyToSubfolders = new Button(I18N.message("applytosubfolders"));
		applyToSubfolders.setAutoFit(true);
		buttons.addMember(applyToSubfolders);

		applyToSubfolders.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.contactingServer();
				AutomationService.Instance.get().applyTriggersToTree(folder.getId(), new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						LD.clearPrompt();
					}
				});
			}
		});

		buttons.setMembers(add, applyToSubfolders);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++) {
			ids[i] = Long.parseLong(selection[i].getAttribute("id"));
		}

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							AutomationService.Instance.get().deleteTriggers(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
								}
							});
						}
					}
				});
			}
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onEdit();
			}
		});

		contextMenu.setItems(edit, delete);
		contextMenu.showContextMenu();
	}

	/**
	 * Updates the selected record with new data
	 * 
	 * @param trigger the trigger to update
	 */
	public void updateRecord(GUIAutomationTrigger trigger) {
		Record record = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, trigger.getId()));
		if (record == null) {
			record = new ListGridRecord();
			// Append a new record
			record.setAttribute("id", trigger.getId());
			list.addData(record);
			list.selectRecord(record);
		}

		record.setAttribute("events", trigger.getEvents() != null ? trigger.getEvents() : "");
		record.setAttribute("automation", trigger.getAutomation() != null ? trigger.getAutomation() : "");

		if (trigger.getRoutine() != null) {
			record.setAttribute("routine", trigger.getRoutine().getName());
			record.setAttribute("routineId", trigger.getRoutine().getId());
		} else {
			record.setAttribute("routine", (String) null);
			record.setAttribute("routineId", (Long) null);
		}

		list.refreshRow(list.getRecordIndex(record));
	}

	private void onEdit() {
		ListGridRecord selection = list.getSelectedRecord();
		AutomationService.Instance.get().getTrigger(selection.getAttributeAsLong("id"),
				new AsyncCallback<GUIAutomationTrigger>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIAutomationTrigger trigger) {
						AutomationTriggerDialog dialog = new AutomationTriggerDialog(trigger,
								FolderAutomationPanel.this);
						dialog.show();
					}
				});
	}
}