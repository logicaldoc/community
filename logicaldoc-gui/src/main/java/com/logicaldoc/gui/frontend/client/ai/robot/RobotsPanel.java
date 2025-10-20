package com.logicaldoc.gui.frontend.client.ai.robot;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panel showing the list of robots
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotsPanel extends AdminPanel {
	private static final String ENABLED = "eenabled";

	private static final String ID = "id";

	static final Canvas SELECT_MODEL = new HTMLPanel("&nbsp;" + I18N.message("selectarobot"));

	private static final String LABEL = "label";

	private static final String DESCRIPTION = "description";

	protected Layout detailsContainer;

	protected RefreshableListGrid list;

	protected Canvas details = SELECT_MODEL;

	public RobotsPanel() {
		super("robots");
	}

	@Override
	public void onDraw() {
		InfoPanel infoPanel = new InfoPanel("");

		final Layout listing = new VLayout();
		detailsContainer = new VLayout();
		details = SELECT_MODEL;

		// Initialize the listing panel
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("55%");
		listing.setShowResizeBar(true);

		ListGridField enabled = new EnabledListGridField();

		ListGridField id = new IdListGridField();

		RobotListGridField name = new RobotListGridField();
		name.setCanFilter(true);
		name.setCanSort(true);
		name.setMinWidth(110);
		name.setAutoFit(AutoFitWidthApproach.BOTH);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL), 200);
		label.setCanFilter(true);
		label.setCanSort(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION), 300);
		description.setCanFilter(true);
		description.setCanSort(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, id, name, label, description);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new RobotsDS());

		listing.addMember(infoPanel);
		listing.addMember(list);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(click -> refresh());
		toolStrip.addButton(refresh);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addrobot"));
		toolStrip.addButton(add);
		add.addClickHandler(event -> onAddRobot());

		toolStrip.addFill();

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addSelectionChangedHandler(event -> {
			Record rec = list.getSelectedRecord();
			if (rec != null)
				RobotService.Instance.get().get(rec.getAttributeAsLong(ID), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIRobot robot) {
						showRobotDetails(robot);
					}
				});
		});

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showrobots", Integer.toString(list.getTotalRows()))));

		detailsContainer.setAlign(Alignment.CENTER);
		detailsContainer.addMember(details);

		body.setMembers(toolStrip, listing, detailsContainer);
	}

	private void refresh() {
		list.refresh(new RobotsDS());
		detailsContainer.removeMembers(detailsContainer.getMembers());
		details = SELECT_MODEL;
		detailsContainer.setMembers(details);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		List<Long> ids = new ArrayList<>();
		for (ListGridRecord rec : selection)
			ids.add(rec.getAttributeAsLong(ID));

		Long selectedRobotId = selection[0].getAttributeAsLong("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				RobotService.Instance.get().delete(ids, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
						showRobotDetails(null);
					}
				});
			}
		}));

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(
				event -> RobotService.Instance.get().enable(selectedRobotId, true, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.getSelectedRecord().setAttribute(ENABLED, true);
						list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
					}
				}));
		enable.setEnabled(!list.getSelectedRecord().getAttributeAsBoolean(ENABLED));

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(
				event -> RobotService.Instance.get().enable(selectedRobotId, false, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						list.getSelectedRecord().setAttribute(ENABLED, false);
						list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
					}
				}));
		disable.setEnabled(list.getSelectedRecord().getAttributeAsBoolean(ENABLED));

		MenuItem ask = new MenuItem();
		ask.setTitle(I18N.message("ask"));
		ask.addClickHandler(event -> RobotService.Instance.get().get(selectedRobotId, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUIRobot robot) {
				LD.askForStringMandatory("question", null, null, question -> RobotThread.get(robot).ask(question));
			}
		}));
		ask.setEnabled(selection[0].getAttributeAsBoolean(ENABLED));

		MenuItem clone = new MenuItem();
		clone.setTitle(I18N.message("clone"));
		clone.addClickHandler(click -> LD.askForString("clone", "name",
				selection[0].getAttributeAsString("name") + "Cloned",
				value -> RobotService.Instance.get().clone(selectedRobotId, value, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIRobot rbt) {
						refresh();
					}
				})));

		contextMenu.setItems(ask, enable, disable, clone, new MenuItemSeparator(), delete);
		contextMenu.showContextMenu();
	}

	protected void showRobotDetails(GUIRobot robot) {
		if (!(details instanceof RobotDetailsPanel)) {
			detailsContainer.removeMember(details);
			details = new RobotDetailsPanel(this);
			detailsContainer.addMember(details);
		}
		((RobotDetailsPanel) details).setRobot(robot);
	}

	public ListGrid getList() {
		return list;
	}

	/**
	 * Updates the selected rec with new data
	 * 
	 * @param robot the robot to take data from
	 */
	public void updateRecord(GUIRobot robot) {
		Record rec = list.find(new AdvancedCriteria(ID, OperatorId.EQUALS, robot.getId()));
		if (rec == null) {
			rec = new ListGridRecord();
			// Append a new rec
			rec.setAttribute(ID, robot.getId());
			list.addData(rec);
			list.selectRecord(rec);
		}

		rec.setAttribute("name", robot.getName());
		rec.setAttribute(LABEL, robot.getLabel() != null ? robot.getLabel() : robot.getName());
		rec.setAttribute(DESCRIPTION, robot.getDescription());
		rec.setAttribute(ENABLED, robot.isEnabled());
		rec.setAttribute("avatar", robot.getAvatar());
		
	}

	protected void onAddRobot() {
		list.deselectAllRecords();
		showRobotDetails(new GUIRobot());
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