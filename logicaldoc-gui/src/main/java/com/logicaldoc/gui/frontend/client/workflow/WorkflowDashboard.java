package com.logicaldoc.gui.frontend.client.workflow;

import java.util.ArrayList;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.widgets.layout.PortalLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Workflow dashboard that displays several portlets like a portal page.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class WorkflowDashboard extends VLayout {

	public static final int TASKS_ASSIGNED = 0;

	public static final int TASKS_I_CAN_OWN = 1;

	public static final int TASKS_SUSPENDED = 2;

	public static final int TASKS_ALL = 3;

	public static final int TASKS_SUPERVISOR = 4;

	public static final int TASKS_INVOLVED = 5;

	private static WorkflowDashboard instance;

	private WorkflowDashlet assignedTasks = null;

	private WorkflowDashlet canOwnTasks = null;

	private WorkflowDashlet adminTasks = null;

	private WorkflowDashlet supervisorTasks = null;

	/*
	 * Portlet that shows the workflows the current user was involved in.
	 */
	private WorkflowDashlet involvedTasks = null;

	private PortalLayout portalLayout = null;

	public WorkflowDashboard() {
		setWidth100();
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> refresh(null));
		ToolStripButton history = new ToolStripButton();
		history.setTitle(I18N.message("history"));
		toolStrip.addButton(history);
		history.addClickHandler(event -> new WorkflowHistoryDialog().show());
		toolStrip.addFill();

		portalLayout = new PortalLayout();
		portalLayout.setShowColumnMenus(false);
		portalLayout.setShowEdges(false);
		portalLayout.setShowShadow(false);
		portalLayout.setCanDrag(false);
		portalLayout.setCanDrop(false);
		portalLayout.setColumnBorder("0px");

		// Place the portlets
		assignedTasks = new WorkflowDashlet(this, TASKS_ASSIGNED);
		portalLayout.addPortlet(assignedTasks, 0, 0);
		canOwnTasks = new WorkflowDashlet(this, TASKS_I_CAN_OWN);
		portalLayout.addPortlet(canOwnTasks, 0, 1);

		if (Menu.enabled(Menu.DASHBOARD_WORKFLOW_ALLWORKFLOWS)) {
			// In case of administrator you see all the workflows
			adminTasks = new WorkflowDashlet(this, TASKS_ALL);
			portalLayout.addPortlet(adminTasks, 1, 0);
		} else if (Menu.enabled(Menu.DASHBOARD_WORKFLOW_WORKFLOWSYOUSUPERVISE)) {
			// Otherwise you see the workflows you are the supervisor
			supervisorTasks = new WorkflowDashlet(this, TASKS_SUPERVISOR);
			portalLayout.addPortlet(supervisorTasks, 1, 1);
		}

		if (Menu.enabled(Menu.DASHBOARD_WORKFLOW_WORKFLOWSINVOLVEDIN)) {
			involvedTasks = new WorkflowDashlet(this, TASKS_INVOLVED);
			portalLayout.addPortlet(involvedTasks, 1, 1);
		}

		addMember(toolStrip);
		addMember(portalLayout);
	}

	/**
	 * Kills the given workflows
	 * 
	 * @param instanceIds Id of the instances to kill
	 */
	public void killWorkflows(ArrayList<String> instanceIds) {
		LD.contactingServer();
		WorkflowService.Instance.get().deleteInstances(instanceIds.toArray(new String[0]), new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				LD.clearPrompt();
			}

			@Override
			public void onSuccess(Void result) {
				refreshGridsAfterWorkflowsKilled(instanceIds);
				LD.clearPrompt();
			}
		});
	}

	private void refreshGridsAfterWorkflowsKilled(ArrayList<String> instanceIds) {
		for (String id : instanceIds) {
			if (assignedTasks != null)
				assignedTasks.onDeletedWorkflow(id);
			if (canOwnTasks != null)
				canOwnTasks.onDeletedWorkflow(id);
			if (adminTasks != null)
				adminTasks.onDeletedWorkflow(id);
			if (supervisorTasks != null)
				supervisorTasks.onDeletedWorkflow(id);
			if (involvedTasks != null)
				involvedTasks.onDeletedWorkflow(id);
		}
	}

	/**
	 * Refreshes all the dashlets related to the given workflow instance
	 * 
	 * @param processId Identifier of the instance(use null for all the
	 *        instances)
	 */
	public void refresh(String processId) {
		if (assignedTasks != null)
			assignedTasks.refresh(processId);
		if (canOwnTasks != null)
			canOwnTasks.refresh(processId);
		if (adminTasks != null)
			adminTasks.refresh(processId);
		if (supervisorTasks != null)
			supervisorTasks.refresh(processId);
		if (involvedTasks != null)
			involvedTasks.refresh(processId);
	}

	public static WorkflowDashboard get() {
		if (instance == null)
			instance = new WorkflowDashboard();
		return instance;
	}
}
