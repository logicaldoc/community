package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.AutomationTriggersDS;
import com.logicaldoc.gui.common.client.grid.EventsListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;

/**
 * This panel shows the automation triggers of a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class FolderAutomationPanel extends FolderDetailTab {

	private static final String ROUTINE = "routine";

	private static final String AUTOMATION = "automation";

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

		ListGridField id = new IdListGridField();

		ListGridField automation = new ListGridField(AUTOMATION, I18N.message(AUTOMATION));
		automation.setWidth("*");
		automation.setCanEdit(false);

		ListGridField routine = new ListGridField(ROUTINE, I18N.message(ROUTINE), 150);
		routine.setCanFilter(true);

		ListGridField events = new EventsListGridField("events", "triggeron");
		events.setWidth(300);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setDataSource(new AutomationTriggersDS(folder.getId(), null));
		list.setFields(id, events, routine, automation);
		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});
		list.addDoubleClickHandler(event -> onEdit());

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
		add.addClickHandler(event -> {
			GUIAutomationTrigger trigger = new GUIAutomationTrigger();
			trigger.setFolder(folder);
			AutomationTriggerDialog dialog = new AutomationTriggerDialog(trigger, FolderAutomationPanel.this);
			dialog.show();
		});

		Button applyToSubfolders = new Button(I18N.message("applytosubfolders"));
		applyToSubfolders.setAutoFit(true);
		buttons.addMember(applyToSubfolders);

		applyToSubfolders.addClickHandler(event -> {
			LD.contactingServer();
			AutomationService.Instance.get().applyTriggersToTree(folder.getId(), new EmptyAsyncCallback<>());
		});

		buttons.setMembers(add, applyToSubfolders);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirmation -> {
					if (Boolean.TRUE.equals(confirmation)) {
						AutomationService.Instance.get().deleteTriggers(GridUtil.getIds(selection),
								new DefaultAsyncCallback<>() {
									@Override
									public void handleSuccess(Void result) {
										list.removeSelectedData();
										list.deselectAllRecords();
									}
								});
					}
				}));

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(event -> onEdit());

		contextMenu.setItems(edit, new MenuItemSeparator(), delete);
		contextMenu.showContextMenu();
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param trigger the trigger to update
	 */
	public void updateRecord(GUIAutomationTrigger trigger) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, trigger.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", trigger.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("events", trigger.getEvents() != null ? trigger.getEvents() : "");
		rec.setAttribute(AUTOMATION, trigger.getAutomation() != null ? trigger.getAutomation() : "");

		if (trigger.getRoutine() != null) {
			rec.setAttribute(ROUTINE, trigger.getRoutine().getName());
			rec.setAttribute("routineId", trigger.getRoutine().getId());
		} else {
			rec.setAttribute(ROUTINE, (String) null);
			rec.setAttribute("routineId", (Long) null);
		}

		list.refreshRow(list.getRecordIndex(rec));
	}

	private void onEdit() {
		ListGridRecord selection = list.getSelectedRecord();
		AutomationService.Instance.get().getTrigger(selection.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUIAutomationTrigger trigger) {
				AutomationTriggerDialog dialog = new AutomationTriggerDialog(trigger, FolderAutomationPanel.this);
				dialog.show();
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