package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.AutomationTriggersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the details of an import folder
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class AutomationTriggersPanel extends VLayout implements FolderChangeListener {

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FOLDER;

	final static Canvas SELECT_FOLDER = new HTMLPanel("&nbsp;" + I18N.message("selecttrigger"));

	private FolderSelector folderSelector;

	private SelectItem event;

	public AutomationTriggersPanel() {
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("45%");
		listing.setShowResizeBar(true);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField triggeron = new ListGridField("events", I18N.message("triggeron"), 300);
		triggeron.setCanFilter(true);
		triggeron.setCellFormatter(new CellFormatter() {

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
							if (string.trim().isEmpty())
								continue;
							labels.add(I18N.message(string.trim() + ".short"));
						}

						String str = labels.toString().substring(1);
						return str.substring(0, str.length() - 1);
					} else if (record.getAttributeAsDate("date") != null) {
						return I18N.formatDate(record.getAttributeAsDate("date"));
					} else if (record.getAttributeAsString("cron") != null) {
						return record.getAttributeAsString("cron");
					} else
						return "";
				} catch (Throwable e) {
					return "";
				}
			}
		});

		ListGridField routine = new ListGridField("routine", I18N.message("routine"), 150);
		routine.setCanFilter(true);

		ListGridField folder = new ListGridField("folder", I18N.message("folder"), 150);
		folder.setCanFilter(true);

		ListGridField automation = new ListGridField("automation", I18N.message("automation"));
		automation.setWidth("*");
		automation.setCanFilter(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, triggeron, folder, routine, automation);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new AutomationTriggersDS(null, null));

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		folderSelector = new FolderSelector("folder", true);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		event = ItemFactory.newEventSelector("event", "event", new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		}, true, true, true, true);
		toolStrip.addFormItem(event);

		toolStrip.addSeparator();
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addtrigger"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.deselectAllRecords();
				GUIAutomationTrigger trigger = new GUIAutomationTrigger();
				showTriggerDetails(trigger);
			}
		});
		toolStrip.addButton(add);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				Record record = list.getSelectedRecord();
				if (record != null)
					AutomationService.Instance.get().getTrigger(record.getAttributeAsLong("id"),
							new AsyncCallback<GUIAutomationTrigger>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAutomationTrigger trigger) {
									showTriggerDetails(trigger);
								}
							});
			}
		});

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showtriggers", Integer.toString(list.getTotalRows())));
			}
		});

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	public void refresh() {
		list.refresh(
				new AutomationTriggersDS(folderSelector.getFolderId() != null ? folderSelector.getFolderId() : null,
						event.getValue() != null ? event.getValueAsString() : null));
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_FOLDER;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord record = list.getSelectedRecord();
		final long id = Long.parseLong(record.getAttributeAsString("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							AutomationService.Instance.get().deleteTriggers(new long[] { id },
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											list.removeSelectedData();
											list.deselectAllRecords();
											showTriggerDetails(null);
										}
									});
						}
					}
				});
			}
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public void showTriggerDetails(GUIAutomationTrigger trigger) {
		if (!(details instanceof AutomationTriggerDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new AutomationTriggerDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((AutomationTriggerDetailsPanel) details).setTrigger(trigger);
	}

	public ListGrid getList() {
		return list;
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

		record.setAttribute("events", trigger.getEvents() != null ? trigger.getEvents() : null);
		record.setAttribute("date", trigger.getDate() != null ? trigger.getDate() : null);
		record.setAttribute("cron",
				trigger.getCron() != null && !trigger.getCron().isEmpty() ? trigger.getCron() : null);

		record.setAttribute("automation", trigger.getAutomation() != null ? trigger.getAutomation() : "");

		if (trigger.getRoutine() != null) {
			record.setAttribute("routine", trigger.getRoutine().getName());
			record.setAttribute("routineId", trigger.getRoutine().getId());
		} else {
			record.setAttribute("routine", (String) null);
			record.setAttribute("routineId", (Long) null);
		}

		if (trigger.getFolder() != null) {
			record.setAttribute("folder", trigger.getFolder().getName());
			record.setAttribute("folderId", trigger.getFolder().getId());
		} else {
			record.setAttribute("folder", (String) null);
			record.setAttribute("folderId", (Long) null);
		}

		list.refreshRow(list.getRecordIndex(record));
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
	}
}