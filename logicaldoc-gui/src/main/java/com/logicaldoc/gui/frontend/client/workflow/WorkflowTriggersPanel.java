package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.WorkflowTriggersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.EventsListGridField;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Displays the list of all workflow triggers on a folder.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowTriggersPanel extends VLayout {

	private ListGrid list;

	private GUIFolder folder;

	public WorkflowTriggersPanel(final GUIFolder folder) {
		this.folder = folder;
	}

	@Override
	protected void onDraw() {
		refresh();

		Button addTrigger = new Button(I18N.message("workflowtriggeradd"));
		addTrigger.setAutoFit(true);

		Button applyTriggersToSubfolders = new Button(I18N.message("applytosubfolders"));
		applyTriggersToSubfolders.setAutoFit(true);
		applyTriggersToSubfolders.addClickHandler(event -> {
			LD.contactingServer();
			WorkflowService.Instance.get().applyTriggersToTree(folder.getId(), new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void v) {
					LD.clearPrompt();
				}
			});
		});

		HLayout buttons = new HLayout();
		buttons.setMembers(addTrigger, applyTriggersToSubfolders);
		buttons.setMembersMargin(3);
		buttons.setWidth100();
		buttons.setHeight(15);

		addTrigger.addClickHandler(event -> {
			list.deselectAllRecords();
			TriggerDialog dialog = new TriggerDialog(WorkflowTriggersPanel.this);
			dialog.show();
		});

		setMembersMargin(4);
		addMember(buttons);
	}

	void refresh() {
		if (list != null)
			removeMember(list);

		ListGridField workflow = new ListGridField("workflow", I18N.message("workflow"), 200);
		workflow.setCanFilter(true);

		ListGridField template = new ListGridField("template", I18N.message("template"), 200);
		template.setCanFilter(true);

		EventsListGridField events = new EventsListGridField("events", I18N.message("triggeron"));

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(false);
		list.setDataSource(new WorkflowTriggersDS("" + folder.getId()));
		list.setFields(workflow, template, events);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		addMember(list, 0);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem deleteTrigger = new MenuItem();
		deleteTrigger.setTitle(I18N.message("ddelete"));
		deleteTrigger.addClickHandler(
				event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
						ListGridRecord rec = list.getSelectedRecord();
						WorkflowService.Instance.get().deleteTrigger(Long.parseLong(rec.getAttributeAsString("id")),
								new AsyncCallback<>() {
									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Void result) {
										removeMember(list);
										refresh();
									}
								});
					}
				}));

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(event -> new TriggerDialog(WorkflowTriggersPanel.this).show());

		contextMenu.setItems(edit, deleteTrigger);
		contextMenu.showContextMenu();
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public ListGridRecord getSelectedRecord() {
		return list.getSelectedRecord();
	}
}