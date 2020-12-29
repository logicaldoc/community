package com.logicaldoc.gui.frontend.client.workflow;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.WorkflowTriggersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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
		applyTriggersToSubfolders.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				WorkflowService.Instance.get().applyTriggersToTree(folder.getId(), new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void v) {
						ContactingServer.get().hide();
					}
				});
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembers(addTrigger, applyTriggersToSubfolders);
		buttons.setMembersMargin(3);
		buttons.setWidth100();
		buttons.setHeight(15);

		addTrigger.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.deselectAllRecords();
				TriggerDialog dialog = new TriggerDialog(WorkflowTriggersPanel.this);
				dialog.show();
			}
		});

		setMembersMargin(4);
		addMember(buttons);
	}

	void refresh() {
		if(list!=null)
			removeMember(list);
		
		ListGridField workflow = new ListGridField("workflow", I18N.message("workflow"), 200);
		workflow.setCanFilter(true);

		ListGridField template = new ListGridField("template", I18N.message("template"), 200);
		template.setCanFilter(true);

		ListGridField events = new ListGridField("events", I18N.message("triggeron"));
		events.setCanFilter(false);
		events.setAlign(Alignment.LEFT);
		events.setWidth("*");
		events.setCanEdit(false);
		events.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				try {
					if (value != null && !value.toString().isEmpty()) {
						// Translate the set of events
						String[] key = null;

						if (!value.toString().contains(","))
							key = new String[] { value.toString().trim() };
						else
							key = value.toString().split(",");
						List<String> labels = new ArrayList<String>();
						for (String string : key) {
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

		list = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				return super.getCellCSSText(record, rowNum, colNum);
			}
		};
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

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		addMember(list, 0);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem deleteTrigger = new MenuItem();
		deleteTrigger.setTitle(I18N.message("ddelete"));
		deleteTrigger.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ListGridRecord record = list.getSelectedRecord();
							WorkflowService.Instance.get().deleteTrigger(
									Long.parseLong(record.getAttributeAsString("id")), new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											removeMember(list);
											refresh();
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
				TriggerDialog dialog = new TriggerDialog(WorkflowTriggersPanel.this);
				dialog.show();
			}
		});

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