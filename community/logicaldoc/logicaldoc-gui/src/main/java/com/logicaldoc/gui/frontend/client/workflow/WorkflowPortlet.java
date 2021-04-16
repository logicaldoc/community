package com.logicaldoc.gui.frontend.client.workflow;

import java.util.ArrayList;

import com.google.gwt.user.client.rpc.AsyncCallback;
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
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.ViewStateChangedEvent;
import com.smartgwt.client.widgets.grid.events.ViewStateChangedHandler;
import com.smartgwt.client.widgets.layout.Portlet;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Portlet specialized in listing user workflow task records.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowPortlet extends Portlet {

	private RefreshableListGrid list;

	private int type = WorkflowDashboard.TASKS_ASSIGNED;

	private WorkflowDashboard workflowDashboard;

	// We save the state of the grid to correctly handle the refreshes
	private String gridState = null;

	private SpinnerItem max = null;

	public WorkflowPortlet(WorkflowDashboard dashboard, int type) {
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
		} else if (type == WorkflowDashboard.TASKS_ADMIN) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("workflowtasksadmin")));
		} else if (type == WorkflowDashboard.TASKS_SUPERVISOR) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("workflowtaskssupervisor")));
		} else if (type == WorkflowDashboard.TASKS_INVOLVED) {
			setTitle(AwesomeFactory.getIconHtml("cogs", I18N.message("workflowsinvolvedin")));
		}

		HeaderControl exportControl = new HeaderControl(HeaderControl.SAVE, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(list, true);
			}
		});
		exportControl.setTooltip(I18N.message("export"));

		HeaderControl printControl = new HeaderControl(HeaderControl.PRINT, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(list);
			}
		});
		printControl.setTooltip(I18N.message("print"));

		DynamicForm maxSelector = new DynamicForm();
		maxSelector.setNumCols(1);
		maxSelector.setLayoutAlign(Alignment.CENTER);

		max = ItemFactory.newSpinnerItem("max", "max", 100);
		max.setMin(0);
		max.setStep(10);
		max.setShowHint(false);
		max.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});
		maxSelector.setItems(max);

		setHeaderControls(HeaderControls.HEADER_LABEL, maxSelector, exportControl, printControl);
	}

	@Override
	public void onDraw() {
		ListGridField workflow = new ListGridField("workflow", I18N.message("workflow"), 100);
		ListGridField id = new ListGridField("id", I18N.message("id"), 70);
		id.setHidden(true);
		ListGridField processId = new ListGridField("processId", I18N.message("processid"), 80);
		processId.setHidden(true);
		ListGridField name = new ListGridField("name", I18N.message("task"), 100);

		ListGridField templateVersion = new ListGridField("templateVersion", I18N.message("version"), 70);
		templateVersion.setHidden(true);

		ListGridField pooledAssignees = new ListGridField("pooledassignees", I18N.message("pooledassignees"), 150);
		ListGridField documents = new ListGridField("documents", I18N.message("documents"), 300);
		ListGridField documentIds = new ListGridField("documentIds", I18N.message("documentids"), 200);
		documentIds.setHidden(true);
		ListGridField tag = new ListGridField("tag", I18N.message("tag"), 120);
		ListGridField lastnote = new ListGridField("lastnote", I18N.message("lastnote"), 120);

		ListGridField startdate = new DateListGridField("startdate", "startdate");

		ListGridField duedate = new DateListGridField("duedate", "duedate");

		ListGridField enddate = new DateListGridField("enddate", "enddate");
		enddate.setHidden(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(true);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setHeight100();
		list.setBorder("0px");
		list.setDataSource(new WorkflowTasksDS(type, null, max.getValueAsInteger()));
		list.sort("startdate", SortDirection.ASCENDING);
		if (type == WorkflowDashboard.TASKS_I_CAN_OWN || type == WorkflowDashboard.TASKS_ADMIN
				|| type == WorkflowDashboard.TASKS_SUPERVISOR || type == WorkflowDashboard.TASKS_INVOLVED)
			list.setFields(workflow, templateVersion, tag, startdate, duedate, enddate, name, id, processId, documents,
					lastnote, documentIds, pooledAssignees);
		else
			list.setFields(workflow, templateVersion, tag, startdate, duedate, enddate, id, processId, name, documents,
					lastnote, documentIds);

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				Record record = event.getRecord();
				WorkflowService.Instance.get().getWorkflowDetailsByTask(record.getAttributeAsString("id"),
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
			}
		});

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		if (type == WorkflowDashboard.TASKS_ASSIGNED)
			// Count the total of user tasks
			list.addDataArrivedHandler(new DataArrivedHandler() {
				@Override
				public void onDataArrived(DataArrivedEvent event) {
					int total = list.getTotalRows();
					Session.get().getUser().setAssignedTasks(total);
					UserController.get().changed(Session.get().getUser());
				}
			});

		/*
		 * Save the layout of the grid at every change
		 */
		list.addViewStateChangedHandler(new ViewStateChangedHandler() {
			@Override
			public void onViewStateChanged(ViewStateChangedEvent event) {
				gridState = list.getViewState();
			}
		});

		/*
		 * Restore any previously saved view state for this grid
		 */
		list.addDrawHandler(new DrawHandler() {
			@Override
			public void onDraw(DrawEvent event) {
				if (gridState != null)
					list.setViewState(gridState);
			}
		});

		addItem(list);
	}

	public void refresh() {
		list.refresh(new WorkflowTasksDS(type, null, max.getValueAsInteger()));
	}

	public void onDeletedWorkflow(String processId) {
		Record[] records = list.findAll(new AdvancedCriteria("processId", OperatorId.EQUALS, processId));
		for (Record record : records)
			list.removeData(record);
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
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							ArrayList<String> ids = new ArrayList<String>();
							ListGridRecord[] selectedRecords = list.getSelectedRecords();
							for (ListGridRecord record : selectedRecords)
								ids.add(record.getAttributeAsString("processId"));
							workflowDashboard.killWorkflows(ids);
						}
					}
				});
			}
		});

		if (!Session.get().getUser().isMemberOf("admin"))
			delete.setEnabled(false);

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}