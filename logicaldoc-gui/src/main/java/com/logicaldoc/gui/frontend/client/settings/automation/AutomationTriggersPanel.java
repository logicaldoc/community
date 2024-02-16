package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.Arrays;

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
import com.logicaldoc.gui.common.client.widgets.grid.EventsListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the details of an import folder
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class AutomationTriggersPanel extends VLayout implements FolderChangeListener {

	private static final String AUTOMATION = "automation";

	private static final String FOLDER = "folder";

	private static final String ROUTINE = "routine";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FOLDER;

	static final Canvas SELECT_FOLDER = new HTMLPanel("&nbsp;" + I18N.message("selecttrigger"));

	private FolderSelector folderSelector;

	private SelectItem event;

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

		ListGridField triggeron = new EventsListGridField("events", "triggeron");
		triggeron.setCanFilter(true);

		ListGridField routine = new ListGridField(ROUTINE, I18N.message(ROUTINE), 150);
		routine.setCanFilter(true);

		ListGridField folder = new ListGridField(FOLDER, I18N.message(FOLDER), 150);
		folder.setCanFilter(true);

		ListGridField automation = new ListGridField(AUTOMATION, I18N.message(AUTOMATION));
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
		refresh.addClickHandler(evnt -> refresh());

		folderSelector = new FolderSelector(FOLDER, null);
		folderSelector.setWrapTitle(false);
		folderSelector.setWidth(250);
		folderSelector.addFolderChangeListener(this);
		toolStrip.addFormItem(folderSelector);

		event = ItemFactory.newEventSelector("event", "event", evnt -> refresh(), true, true, true, true, true);
		toolStrip.addFormItem(event);

		toolStrip.addSeparator();
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addtrigger"));
		add.addClickHandler(evnt -> {
			list.deselectAllRecords();
			GUIAutomationTrigger trigger = new GUIAutomationTrigger();
			showTriggerDetails(trigger);
		});
		toolStrip.addButton(add);

		list.addCellContextClickHandler(evnt -> {
			showContextMenu();
			evnt.cancel();
		});

		list.addSelectionChangedHandler(evnt -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				AutomationService.Instance.get().getTrigger(rec.getAttributeAsLong("id"),
						new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIAutomationTrigger trigger) {
								showTriggerDetails(trigger);
							}
						});
		});

		list.addDataArrivedHandler(
				evnt -> infoPanel.setMessage(I18N.message("showtriggers", Integer.toString(list.getTotalRows()))));

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

		final ListGridRecord rec = list.getSelectedRecord();
		final long id = rec.getAttributeAsLong("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(evnt -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
			if (Boolean.TRUE.equals(value)) {
				AutomationService.Instance.get().deleteTriggers(Arrays.asList(id), new AsyncCallback<>() {
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
		}));

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

		rec.setAttribute("events", trigger.getEvents() != null ? trigger.getEvents() : null);
		rec.setAttribute("date", trigger.getDate() != null ? trigger.getDate() : null);
		rec.setAttribute("cron", trigger.getCron() != null && !trigger.getCron().isEmpty() ? trigger.getCron() : null);

		rec.setAttribute(AUTOMATION, trigger.getAutomation() != null ? trigger.getAutomation() : "");

		if (trigger.getRoutine() != null) {
			rec.setAttribute(ROUTINE, trigger.getRoutine().getName());
			rec.setAttribute("routineId", trigger.getRoutine().getId());
		} else {
			rec.setAttribute(ROUTINE, (String) null);
			rec.setAttribute("routineId", (Long) null);
		}

		if (trigger.getFolder() != null) {
			rec.setAttribute(FOLDER, trigger.getFolder().getName());
			rec.setAttribute("folderId", trigger.getFolder().getId());
		} else {
			rec.setAttribute(FOLDER, (String) null);
			rec.setAttribute("folderId", (Long) null);
		}

		list.refreshRow(list.getRecordIndex(rec));
	}

	@Override
	public void onChanged(GUIFolder folder) {
		refresh();
	}
}