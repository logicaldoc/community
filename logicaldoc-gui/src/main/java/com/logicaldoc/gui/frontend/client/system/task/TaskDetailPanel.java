package com.logicaldoc.gui.frontend.client.system.task;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.logicaldoc.gui.frontend.client.system.LogPanel;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all tasks details
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 * 
 */
public class TaskDetailPanel extends VLayout {

	private EditingTabSet tabSet;

	private Layout schedulingTabPanel;

	private Layout logTabPanel;

	private Layout notificationTabPanel;

	private GUITask task;

	private TaskSchedulingPanel schedulingPanel;

	private LogPanel logPanel;

	private TaskNotificationPanel notificationPanel;

	private TasksPanel tasksPanel;

	public TaskDetailPanel(TasksPanel tasksPanel) {
		super();
		this.tasksPanel = tasksPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);
	}

	@Override
	public void onDraw() {
		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> SystemService.Instance.get()
				.getTaskByName(task.getName(), I18N.getLocale(), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUITask task) {
						setTask(task);
						tabSet.hideSave();
					}
				}));

		Tab schedulingTab = new Tab(I18N.message("scheduling"));
		schedulingTabPanel = new HLayout();
		schedulingTabPanel.setWidth100();
		schedulingTabPanel.setHeight100();
		schedulingTab.setPane(schedulingTabPanel);

		Tab notificationTab = new Tab(I18N.message("notifications"));
		notificationTabPanel = new HLayout();
		notificationTabPanel.setWidth100();
		notificationTabPanel.setHeight100();
		if (Feature.enabled(Feature.TASK_REPORT_NOTIFICATION))
			notificationTab.setPane(notificationTabPanel);
		else
			notificationTab.setPane(new FeatureDisabled());

		Tab logTab = new Tab(I18N.message("log"));
		logTabPanel = new HLayout();
		logTabPanel.setWidth100();
		logTabPanel.setHeight100();
		logTab.setPane(logTabPanel);

		if (Session.get().isDefaultTenant()) {
			if (Feature.visible(Feature.TASK_REPORT_NOTIFICATION))
				tabSet.setTabs(schedulingTab, notificationTab, logTab);
			else
				tabSet.setTabs(schedulingTab, logTab);
		} else {
			tabSet.setTabs(logTab);
		}

		addMember(tabSet);
		refresh();
	}

	public GUITask getTask() {
		return task;
	}

	public void setTask(GUITask task) {
		this.task = task;
		refresh();
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the scheduling tab
		 */
		if (schedulingPanel != null) {
			schedulingPanel.destroy();
			if (Boolean.TRUE.equals(schedulingTabPanel.contains(schedulingPanel)))
				schedulingTabPanel.removeMember(schedulingPanel);
		}
		ChangedHandler changeHandler = (ChangedEvent event) -> onModified();
		schedulingPanel = new TaskSchedulingPanel(task, changeHandler);
		schedulingTabPanel.addMember(schedulingPanel);

		/*
		 * Prepare the notifications tab
		 */
		if (notificationPanel != null) {
			notificationPanel.destroy();
			if (Boolean.TRUE.equals(notificationTabPanel.contains(notificationPanel)))
				notificationTabPanel.removeMember(notificationPanel);
		}
		notificationPanel = new TaskNotificationPanel(task, changeHandler);
		notificationTabPanel.addMember(notificationPanel);

		/*
		 * Prepare the log tab
		 */
		if (logPanel != null) {
			logPanel.destroy();
			if (Boolean.TRUE.equals(logTabPanel.contains(logPanel)))
				logTabPanel.removeMember(logPanel);
		}
		logPanel = new LogPanel(task.getName() + "_WEB");
		logTabPanel.addMember(logPanel);
	}

	public void onModified() {
		tabSet.displaySave();
	}

	public void onSave() {
		if (schedulingPanel.validate() && notificationPanel.validate()) {
			SystemService.Instance.get().saveTask(task, I18N.getLocale(), new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUITask task) {
					if (task != null) {
						tasksPanel.updateSelectedRecord(task);
						tabSet.hideSave();
					} else {
						GuiLog.error(I18N.message("genericerror"), null, null);
					}
				}
			});
		}
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