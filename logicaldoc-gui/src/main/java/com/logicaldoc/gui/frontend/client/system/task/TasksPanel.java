package com.logicaldoc.gui.frontend.client.system.task;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.data.TasksDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the list of scheduled tasks
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TasksPanel extends AdminPanel {

	private static final String COMPLETION = "completion";

	private static final String SCHEDULING = "scheduling";

	private static final String IDLE_TASK = "idle_task";

	private static final String ENABLED_ICON = "enabledIcon";

	private static final String RUNNING_ICON = "runningIcon";

	private static final String EENABLED = "eenabled";

	private static final String RUNNING_TASK = "running_task";

	private static final String STATUS = "status";

	private Layout results = new VLayout();

	private Layout details;

	private ListGrid tasksGrid;

	private Timer timer;

	private Canvas detailPanel;

	private Map<String, Progressbar> progresses = new HashMap<>();

	public TasksPanel() {
		super("scheduledtasks");
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem taskExecution = new MenuItem();
		taskExecution.setTitle(I18N.message("execute"));
		taskExecution.addClickHandler(event -> SystemService.Instance.get().getTaskByName(
				tasksGrid.getSelectedRecord().getAttributeAsString("name"), I18N.getLocale(),
				new AsyncCallback<GUITask>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUITask task) {
						final GUITask currentTask = task;
						SystemService.Instance.get().startTask(currentTask.getName(), new AsyncCallback<Boolean>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Boolean result) {
								final ListGridRecord rec = tasksGrid.getSelectedRecord();
								rec.setAttribute(STATUS, GUITask.STATUS_RUNNING);
								rec.setAttribute(RUNNING_ICON, RUNNING_TASK);
								Date now = new Date();
								rec.setAttribute("lastStart", now);
								rec.setAttribute("nextStart",
										new Date(now.getTime() + (currentTask.getScheduling().getInterval() * 1000)));
								tasksGrid.refreshRow(tasksGrid.getRecordIndex(rec));
							}
						});
					}
				}));

		if (GUITask.STATUS_IDLE != tasksGrid.getSelectedRecord().getAttributeAsInt(STATUS)
				|| Boolean.FALSE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(EENABLED)))
			taskExecution.setEnabled(false);

		MenuItem taskStop = new MenuItem();
		taskStop.setTitle(I18N.message("stop"));
		taskStop.addClickHandler(event -> SystemService.Instance.get()
				.stopTask(tasksGrid.getSelectedRecord().getAttributeAsString("name"), new AsyncCallback<Boolean>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Boolean result) {
						tasksGrid.getSelectedRecord().setAttribute(STATUS, GUITask.STATUS_STOPPING);
						tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
					}
				}));

		if (GUITask.STATUS_RUNNING != tasksGrid.getSelectedRecord().getAttributeAsInt(STATUS)
				|| Boolean.FALSE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(EENABLED)))
			taskStop.setEnabled(false);

		MenuItem enableTask = new MenuItem();
		enableTask.setTitle(I18N.message("enable"));
		enableTask.addClickHandler(event -> SystemService.Instance.get()
				.enableTask(tasksGrid.getSelectedRecord().getAttributeAsString("name"), new AsyncCallback<Boolean>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Boolean result) {
						tasksGrid.getSelectedRecord().setAttribute(ENABLED_ICON, "bullet_green");
						tasksGrid.getSelectedRecord().setAttribute(EENABLED, true);
						tasksGrid.getSelectedRecord().setAttribute(RUNNING_ICON, IDLE_TASK);
						tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
					}
				}));

		if (Boolean.TRUE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(EENABLED)))
			enableTask.setEnabled(false);

		MenuItem disableTask = new MenuItem();
		disableTask.setTitle(I18N.message("disable"));
		disableTask.addClickHandler(event -> SystemService.Instance.get()
				.disableTask(tasksGrid.getSelectedRecord().getAttributeAsString("name"), new AsyncCallback<Boolean>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Boolean result) {
						tasksGrid.getSelectedRecord().setAttribute(ENABLED_ICON, "bullet_red");
						tasksGrid.getSelectedRecord().setAttribute(EENABLED, false);
						tasksGrid.getSelectedRecord().setAttribute(RUNNING_ICON, IDLE_TASK);
						tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
					}
				}));

		if (Boolean.FALSE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(EENABLED))
				|| tasksGrid.getSelectedRecord().getAttributeAsInt(STATUS) != GUITask.STATUS_IDLE)
			disableTask.setEnabled(false);

		contextMenu.setItems(taskExecution, taskStop, enableTask, disableTask);
		contextMenu.showContextMenu();
	}

	private void prepareTasksGrid() {
		tasksGrid = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord rec, Integer colNum) {
				String fieldName = this.getFieldName(colNum);
				if (fieldName.equals("progressbar")) {
					Progressbar prgBar = new Progressbar();
					prgBar.setLength(100);
					prgBar.setBreadth(15);
					progresses.put(rec.getAttribute("name"), prgBar);

					return prgBar;
				} else {
					return null;
				}
			}
		};
		tasksGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		tasksGrid.setShowRecordComponents(true);
		tasksGrid.setShowRecordComponentsByCell(true);
		tasksGrid.setAutoFetchData(true);

		ListGridField enabled = new ListGridField(ENABLED_ICON, " ", 30);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".png");
		enabled.setCanFilter(false);

		ListGridField running = new ListGridField(RUNNING_ICON, " ", 30);
		running.setType(ListGridFieldType.IMAGE);
		running.setCanSort(false);
		running.setAlign(Alignment.CENTER);
		running.setImageURLPrefix(Util.imagePrefix());
		running.setImageURLSuffix(".gif");
		running.setCanFilter(false);

		ListGridField label = new ListGridField("label", I18N.message("task"), 180);

		ListGridField description = new ListGridField("description", I18N.message("description"));
		description.setWidth("*");

		ListGridField lastStart = new DateListGridField("lastStart", "laststart");

		ListGridField nextStart = new DateListGridField("nextStart", "nextstart");

		ListGridField scheduling = new ListGridField(SCHEDULING, I18N.message(SCHEDULING), 130);
		scheduling.setCanFilter(false);
		scheduling.setAlign(Alignment.CENTER);
		scheduling.setCanSort(false);

		ListGridField progressbar = new ListGridField("progressbar", I18N.message("progress"), 110);
		progressbar.setCanFilter(false);
		progressbar.setCanSort(false);
		progressbar.setAlign(Alignment.LEFT);
		progressbar.setCellFormatter((value, rec, rowNum, colNum) -> "");

		ListGridField completion = new ListGridField(COMPLETION, " ", 35);
		completion.setCanFilter(false);
		completion.setCanSort(false);
		completion.setAlign(Alignment.LEFT);
		completion.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (value != null)
				return value + "%";
			else
				return "";
		});

		tasksGrid.setWidth100();
		tasksGrid.setHeight100();
		tasksGrid.setFields(enabled, running, label, description, lastStart, nextStart, scheduling, progressbar,
				completion);
		tasksGrid.setSelectionType(SelectionStyle.SINGLE);
		tasksGrid.setShowRecordComponents(true);
		tasksGrid.setShowRecordComponentsByCell(true);
		tasksGrid.setCanFreezeFields(true);
		tasksGrid.setFilterOnKeypress(true);
		tasksGrid.setDataSource(new TasksDS());

		results.addMember(tasksGrid, 1);

		addGridContextHandler();

		addGridSelectionHandler();
	}

	private void addGridSelectionHandler() {
		tasksGrid.addSelectionChangedHandler(event -> {
			ListGridRecord rec = tasksGrid.getSelectedRecord();
			if (rec != null)
				SystemService.Instance.get().getTaskByName(rec.getAttribute("name"), I18N.getLocale(),
						new AsyncCallback<GUITask>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUITask task) {
								onSelectedTask(task);
							}
						});
		});
	}

	private void addGridContextHandler() {
		tasksGrid.addCellContextClickHandler(event -> {
			event.cancel();
			if (!Session.get().isDefaultTenant())
				return;

			final ListGridRecord rec = tasksGrid.getSelectedRecord();
			if (rec != null)
				SystemService.Instance.get().getTaskByName(rec.getAttribute("name"), I18N.getLocale(),
						new AsyncCallback<GUITask>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUITask task) {
								rec.setAttribute(STATUS, task.getStatus());
								rec.setAttribute(EENABLED, task.getScheduling().isEnabled());
								if (task.getStatus() != GUITask.STATUS_IDLE) {
									rec.setAttribute(RUNNING_ICON, RUNNING_TASK);
								} else {
									rec.setAttribute(RUNNING_ICON, IDLE_TASK);
								}
								tasksGrid.refreshRow(tasksGrid.getRecordIndex(rec));
								showContextMenu();
							}
						});
		});
	}

	@Override
	public void onDraw() {
		results.setShowResizeBar(true);
		results.setHeight("60%");
		body.addMember(results);

		detailPanel = new Label("&nbsp;" + I18N.message("selecttask"));
		details = new VLayout();
		details.setAlign(Alignment.CENTER);
		details.addMember(detailPanel);
		body.addMember(details);

		prepareTasksGrid();

		Tab jobs = new Tab();
		jobs.setTitle(I18N.message("jobs"));
		jobs.setPane(new JobsPanel());
		tabs.addTab(jobs);

		/*
		 * Create the timer that synchronize the view
		 */
		timer = new Timer() {
			public void run() {
				loadTasks();
			}
		};
		timer.scheduleRepeating(5 * 1000);
	}

	@Override
	public void destroy() {
		super.destroy();
		this.timer.cancel();
	}

	/**
	 * Shows the task details
	 * 
	 * @param task The task
	 */
	public void onSelectedTask(GUITask task) {
		if (!(detailPanel instanceof TaskDetailPanel)) {
			details.removeMember(detailPanel);
			detailPanel.destroy();
			detailPanel = new TaskDetailPanel(this);
			details.addMember(detailPanel);
		}
		((TaskDetailPanel) detailPanel).setTask(task);
	}

	public ListGrid getList() {
		return tasksGrid;
	}

	/**
	 * Updates the selected rec with the new task data
	 * 
	 * @param task the task to update
	 */
	public void updateSelectedRecord(GUITask task) {
		tasksGrid.getSelectedRecord().setAttribute(SCHEDULING, task.getSchedulingLabel());
		tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
	}

	private void loadTasks() {
		SystemService.Instance.get().loadTasks(I18N.getLocale(), new AsyncCallback<GUITask[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUITask[] tasks) {
				for (GUITask guiTask : tasks) {
					Progressbar p = progresses.get(guiTask.getName());
					if (p == null)
						continue;

					if (guiTask.isIndeterminate()) {
						p.setPercentDone(0);
					} else {
						p.setPercentDone(guiTask.getCompletionPercentage());
					}
					p.redraw();

					updateRecords(guiTask);
				}
			}
		});
	}

	private void updateRecords(GUITask guiTask) {
		for (ListGridRecord rec : tasksGrid.getRecords()) {
			if (rec.getAttribute("name").equals(guiTask.getName()) && guiTask.getStatus() != GUITask.STATUS_IDLE) {
				rec.setAttribute(RUNNING_ICON, RUNNING_TASK);
				rec.setAttribute(COMPLETION, guiTask.getCompletionPercentage());
				tasksGrid.refreshRow(tasksGrid.getRecordIndex(rec));
				break;
			} else if (rec.getAttribute("name").equals(guiTask.getName())
					&& guiTask.getStatus() == GUITask.STATUS_IDLE) {
				rec.setAttribute(RUNNING_ICON, IDLE_TASK);
				rec.setAttribute(COMPLETION, guiTask.getCompletionPercentage());
				tasksGrid.refreshRow(tasksGrid.getRecordIndex(rec));
				break;
			}
		}
	}
}