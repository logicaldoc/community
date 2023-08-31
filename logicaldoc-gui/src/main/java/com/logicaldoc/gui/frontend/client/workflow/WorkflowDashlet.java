package com.logicaldoc.gui.frontend.client.workflow;

import java.util.ArrayList;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowTasksDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.UserController;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Portlet;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Dashlet specialized in listing user workflow task records.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowDashlet extends Portlet {

	private static final String STARTDATE = "startdate";

	private static final String PROCESS_ID = "processId";

	private RefreshableListGrid list;

	private int type = WorkflowDashboard.TASKS_ASSIGNED;

	private WorkflowDashboard workflowDashboard;

	// We save the state of the grid to correctly handle the refreshes
	private String gridState = null;

	private SpinnerItem max = null;

	public WorkflowDashlet(WorkflowDashboard dashboard, int type) {
		this.workflowDashboard = dashboard;
		this.type = type;

		setCanDrag(false);
		setCanDrop(false);
		setDragAppearance(DragAppearance.OUTLINE);
		setDragOpacity(30);

		if (type == WorkflowDashboard.TASKS_ASSIGNED) {
			setTitle(AwesomeFactory.getIconHtml("tasks", I18N.message("workflowtasksassigned")));
		} else if (type == WorkflowDashboard.TASKS_I_CAN_OWN) {
			setTitle(AwesomeFactory.getIconHtml("tasks", I18N.message("workflowtaskspooled")));
		} else if (type == WorkflowDashboard.TASKS_SUSPENDED) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("workflowtaskssuspended")));
		} else if (type == WorkflowDashboard.TASKS_ALL) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("allworkflows")));
		} else if (type == WorkflowDashboard.TASKS_SUPERVISOR) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("workflowsyousupervise")));
		} else if (type == WorkflowDashboard.TASKS_INVOLVED) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("workflowsinvolvedin")));
		}

		HeaderControl exportControl = new HeaderControl(HeaderControl.SAVE, event -> GridUtil.exportCSV(list, true));
		exportControl.setTooltip(I18N.message("export"));

		HeaderControl printControl = new HeaderControl(HeaderControl.PRINT, event -> GridUtil.print(list));
		printControl.setTooltip(I18N.message("print"));

		HeaderControl refreshControl = new HeaderControl(HeaderControl.REFRESH, event -> refresh(null));
		refreshControl.setTooltip(I18N.message("refresh"));

		DynamicForm maxSelector = new DynamicForm();
		maxSelector.setNumCols(1);
		maxSelector.setLayoutAlign(Alignment.CENTER);

		max = ItemFactory.newSpinnerItem("max", Session.get().getConfigAsInt("gui.wf.dashlet.rows"));
		max.setMin(0);
		max.setStep(10);
		max.setShowHint(false);
		max.addChangedHandler(event -> refresh(null));
		maxSelector.setItems(max);

		setHeaderControls(HeaderControls.HEADER_LABEL, maxSelector, refreshControl, exportControl, printControl);
	}

	@Override
	public void onDraw() {
		ListGridField workflowDisplay = new ListGridField("workflowLabel", I18N.message("workflow"), 100);
		ListGridField workflow = new ListGridField("workflow", I18N.message("workflowname"), 100);
		workflow.setHidden(true);
		ListGridField id = new ListGridField("id", I18N.message("id"), 70);
		id.setHidden(true);
		ListGridField processId = new ListGridField(PROCESS_ID, I18N.message("processid"), 80);
		processId.setHidden(true);
		ListGridField name = new WorkflowTaskNameListGridField("name", "display", "task", 100);

		ListGridField templateVersion = new VersionListGridField("templateVersion", "version");
		templateVersion.setHidden(true);

		ListGridField pooledAssignees = new ListGridField("pooledassignees", I18N.message("pooledassignees"), 150);
		ListGridField documents = new ListGridField("documents", I18N.message("documents"), 300);
		ListGridField documentIds = new ListGridField("documentIds", I18N.message("documentids"), 200);
		documentIds.setHidden(true);
		ListGridField tag = new ListGridField("tag", I18N.message("tag"), 120);

		ListGridField lastnote = new ListGridField("lastnote", I18N.message("lastnote"), 120);

		DateListGridField startdate = new DateListGridField(STARTDATE, STARTDATE);

		ListGridField duedate = new DateListGridField("duedate", "duedate");

		ListGridField enddate = new DateListGridField("enddate", "enddate");
		enddate.setHidden(true);

		list = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				String color = record.getAttributeAsString("wfcolor");
				if (color != null && !color.isEmpty()) {
					return "background-color:" + color;
				} else {
					return super.getCellCSSText(record, rowNum, colNum);
				}
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(true);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setHeight100();
		list.setBorder("0px");
		list.setDataSource(new WorkflowTasksDS(type, null, max.getValueAsInteger()));
		list.sort(STARTDATE, SortDirection.ASCENDING);

		if (type == WorkflowDashboard.TASKS_I_CAN_OWN || type == WorkflowDashboard.TASKS_ALL
				|| type == WorkflowDashboard.TASKS_SUPERVISOR || type == WorkflowDashboard.TASKS_INVOLVED)
			list.setFields(workflow, workflowDisplay, templateVersion, tag, startdate, duedate, enddate, name, id,
					processId, documents, lastnote, documentIds, pooledAssignees);
		else
			list.setFields(workflow, workflowDisplay, templateVersion, tag, startdate, duedate, enddate, id, processId,
					name, documents, lastnote, documentIds);

		list.addCellDoubleClickHandler(event -> {
			Record rec = event.getRecord();
			WorkflowService.Instance.get().getWorkflowDetailsByTask(rec.getAttributeAsString("id"),
					new AsyncCallback<GUIWorkflow>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIWorkflow result) {
							if (result != null) {
								TaskDetailsDialog workflowDetailsDialog = new TaskDetailsDialog(workflowDashboard,
										result, type == WorkflowDashboard.TASKS_INVOLVED);
								workflowDetailsDialog.show();
							}
						}
					});
		});

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		/*
		 * Save the layout of the grid at every change
		 */
		list.addViewStateChangedHandler(event -> gridState = list.getViewState());

		/*
		 * Restore any previously saved view state for this grid
		 */
		list.addDrawHandler(event -> {
			if (gridState != null)
				list.setViewState(gridState);
		});

		if (type == WorkflowDashboard.TASKS_ASSIGNED)
			countTotalAssignedTasksToCurrentUser();

		addItem(list);
	}

	private void countTotalAssignedTasksToCurrentUser() {
		// Count the total of user tasks
		list.addDataArrivedHandler(event -> {
			int total = list.getTotalRows();

			// If the returned entries are a different total,
			// recalculate the total assigned tasks
			if (total != Session.get().getUser().getAssignedTasks()) {
				WorkflowService.Instance.get().countAssignedTasks(Session.get().getUser().getUsername(),
						new AsyncCallback<Integer>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Integer total) {
								Session.get().getUser().setAssignedTasks(total != null ? total.intValue() : 0);
								UserController.get().changed(Session.get().getUser());
							}
						});

			}
		});
	}

	public void refresh(String processId) {
		if (processId == null || processId.isEmpty()
				|| list.find(new AdvancedCriteria(PROCESS_ID, OperatorId.EQUALS, processId)) != null)
			list.refresh(new WorkflowTasksDS(type, null, max.getValueAsInteger()));
	}

	public void onDeletedWorkflow(String processId) {
		Record[] records = list.findAll(new AdvancedCriteria(PROCESS_ID, OperatorId.EQUALS, processId));
		for (Record rec : records)
			list.removeData(rec);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord selection = list.getSelectedRecord();
		if (selection == null)
			return;

		/*
		 * This command will delete the Workflow instance of the currently
		 * selected task
		 */
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
			if (Boolean.TRUE.equals(value)) {
				ArrayList<String> ids = new ArrayList<>();
				ListGridRecord[] selectedRecords = list.getSelectedRecords();
				for (ListGridRecord rec : selectedRecords)
					ids.add(rec.getAttributeAsString(PROCESS_ID));
				workflowDashboard.killWorkflows(ids);
			}
		}));

		if (!Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN))
			delete.setEnabled(false);

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public WorkflowDashboard getWorkflowDashboard() {
		return workflowDashboard;
	}
}