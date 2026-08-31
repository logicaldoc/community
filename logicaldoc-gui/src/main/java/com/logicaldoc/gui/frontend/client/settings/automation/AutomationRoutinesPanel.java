package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.data.AutomationRoutinesDS;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
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
public class AutomationRoutinesPanel extends VLayout {

	private static final String AUTOMATION = "automation";

	private static final String DESCRIPTION = "description";

	private Layout detailsContainer = new VLayout();

	private RefreshableListGrid list;

	private Canvas details = SELECT_FOLDER;

	static final Canvas SELECT_FOLDER = new HTMLPanel("&nbsp;" + I18N.message("selectroutine"));

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		// Initialize the listing panel
		Layout listing = new VLayout();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("50%");
		listing.setShowResizeBar(true);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"), 150);
		name.setCanFilter(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 200);
		description.setCanFilter(true);

		ListGridField automation = new ListGridField(AUTOMATION, I18N.message(AUTOMATION));
		automation.setWidth("*");
		automation.setCanFilter(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(id, name, description, automation);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new AutomationRoutinesDS(false));
		list.setSortField("name");

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler((ClickEvent event) -> refresh());

		toolStrip.addSeparator();
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addroutine"));
		add.addClickHandler((ClickEvent event) -> {
			list.deselectAllRecords();
			GUIAutomationRoutine routine = new GUIAutomationRoutine();
			showRoutineDetails(routine);
		});
		toolStrip.addButton(add);

		list.addCellContextClickHandler((CellContextClickEvent event) -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler((SelectionEvent event) -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				AutomationService.Instance.get().getRoutine(rec.getAttributeAsLong("id"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIAutomationRoutine routine) {
						showRoutineDetails(routine);
					}
				});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showroutines", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		setMembers(toolStrip, listing, detailsContainer);
	}

	public void refresh() {
		list.refresh(new AutomationRoutinesDS(false));
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
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				AutomationService.Instance.get().deleteRoutines(Arrays.asList(id), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showRoutineDetails(null);
					}
				});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public void showRoutineDetails(GUIAutomationRoutine routine) {
		if (!(details instanceof AutomationRoutineDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new AutomationRoutineDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((AutomationRoutineDetailsPanel) details).setRoutine(routine);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param routine the routine to update
	 */
	public void updateRecord(GUIAutomationRoutine routine) {
		Record rec = list.find(new AdvancedCriteria("id", OperatorId.EQUALS, routine.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute("id", routine.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", routine.getName());
		rec.setAttribute(DESCRIPTION, routine.getDescription());
		rec.setAttribute(AUTOMATION, routine.getAutomation());

		list.refreshRow(list.getRecordIndex(rec));
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