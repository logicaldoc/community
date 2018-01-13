package com.logicaldoc.gui.frontend.client.system.task;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.logicaldoc.gui.frontend.client.system.LogPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects all tasks details
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 * 
 */
public class TaskDetailPanel extends VLayout {

	private TabSet tabSet = new TabSet();

	private Layout schedulingTabPanel;

	private Layout logTabPanel;

	private Layout notificationTabPanel;

	private GUITask task;

	private TaskSchedulingPanel schedulingPanel;

	private LogPanel logPanel;

	private TaskNotificationPanel notificationPanel;

	private HLayout savePanel;

	private TasksPanel tasksPanel;

	public TaskDetailPanel(TasksPanel tasksPanel) {
		super();
		this.tasksPanel = tasksPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		Button saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(2);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("70%");
		spacer.setOverflow(Overflow.HIDDEN);

		Img closeImage = ItemFactory.newImgIcon("delete.png");
		closeImage.setHeight("16px");
		closeImage.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SystemService.Instance.get().getTaskByName(task.getName(), I18N.getLocale(),
						new AsyncCallback<GUITask>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUITask task) {
								setTask(task);
								savePanel.setVisible(false);
							}
						});
			}
		});
		closeImage.setCursor(Cursor.HAND);
		closeImage.setTooltip(I18N.message("close"));
		closeImage.setLayoutAlign(Alignment.RIGHT);
		closeImage.setLayoutAlign(VerticalAlignment.CENTER);

		savePanel.addMember(saveButton);
		savePanel.addMember(spacer);
		savePanel.addMember(closeImage);
		addMember(savePanel);

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
	}

	public GUITask getTask() {
		return task;
	}

	public void setTask(GUITask task) {
		this.task = task;
		refresh();
	}

	private void refresh() {
		if (savePanel != null)
			savePanel.setVisible(false);

		/*
		 * Prepare the scheduling tab
		 */
		if (schedulingPanel != null) {
			schedulingPanel.destroy();
			if (schedulingTabPanel.contains(schedulingPanel))
				schedulingTabPanel.removeMember(schedulingPanel);
		}
		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};
		schedulingPanel = new TaskSchedulingPanel(task, changeHandler);
		schedulingTabPanel.addMember(schedulingPanel);

		/*
		 * Prepare the notifications tab
		 */
		if (notificationPanel != null) {
			notificationPanel.destroy();
			if (notificationTabPanel.contains(notificationPanel))
				notificationTabPanel.removeMember(notificationPanel);
		}
		notificationPanel = new TaskNotificationPanel(task, changeHandler);
		notificationTabPanel.addMember(notificationPanel);

		/*
		 * Prepare the log tab
		 */
		if (logPanel != null) {
			logPanel.destroy();
			if (logTabPanel.contains(logPanel))
				logTabPanel.removeMember(logPanel);
		}
		logPanel = new LogPanel(task.getName() + "_WEB");
		logTabPanel.addMember(logPanel);
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	public void onSave() {
		if (schedulingPanel.validate() && notificationPanel.validate()) {
			SystemService.Instance.get().saveTask(task, I18N.getLocale(), new AsyncCallback<GUITask>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUITask task) {
					if (task != null) {
						tasksPanel.updateSelectedRecord(task);
						savePanel.setVisible(false);
					} else {
						Log.error(I18N.message("genericerror"), null, null);
					}
				}
			});
		}
	}
}