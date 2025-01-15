package com.logicaldoc.gui.frontend.client.system.task;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.data.TasksDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RunningListGridField;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
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

	private static final String ENABLED = "eenabled";

	private static final String RUNNING = "running";

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
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUITask task) {
						final GUITask currentTask = task;
						SystemService.Instance.get().startTask(currentTask.getName(), new DefaultAsyncCallback<Void>() {
							@Override
							public void onSuccess(Void result) {
								final ListGridRecord rec = tasksGrid.getSelectedRecord();
								rec.setAttribute(STATUS, GUITask.STATUS_RUNNING);
								rec.setAttribute(RUNNING, true);
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
				|| Boolean.FALSE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(ENABLED)))
			taskExecution.setEnabled(false);

		MenuItem taskStop = new MenuItem();
		taskStop.setTitle(I18N.message("stop"));
		taskStop.addClickHandler(event -> SystemService.Instance.get()
				.stopTask(tasksGrid.getSelectedRecord().getAttributeAsString("name"), new DefaultAsyncCallback<Void>() {
					@Override
					public void onSuccess(Void result) {
						tasksGrid.getSelectedRecord().setAttribute(STATUS, GUITask.STATUS_STOPPING);
						tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
					}
				}));

		if (GUITask.STATUS_RUNNING != tasksGrid.getSelectedRecord().getAttributeAsInt(STATUS)
				|| Boolean.FALSE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(ENABLED)))
			taskStop.setEnabled(false);

		MenuItem enableTask = new MenuItem();
		enableTask.setTitle(I18N.message("enable"));
		enableTask.addClickHandler(event -> SystemService.Instance.get()
				.enableTask(tasksGrid.getSelectedRecord().getAttributeAsString("name"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Boolean result) {
						tasksGrid.getSelectedRecord().setAttribute(ENABLED, true);
						tasksGrid.getSelectedRecord().setAttribute(RUNNING, false);
						tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
					}
				}));

		if (Boolean.TRUE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(ENABLED)))
			enableTask.setEnabled(false);

		MenuItem disableTask = new MenuItem();
		disableTask.setTitle(I18N.message("disable"));
		disableTask.addClickHandler(event -> SystemService.Instance.get()
				.disableTask(tasksGrid.getSelectedRecord().getAttributeAsString("name"), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Boolean result) {
						tasksGrid.getSelectedRecord().setAttribute(ENABLED, false);
						tasksGrid.getSelectedRecord().setAttribute(RUNNING, false);
						tasksGrid.refreshRow(tasksGrid.getRecordIndex(tasksGrid.getSelectedRecord()));
					}
				}));

		if (Boolean.FALSE.equals(tasksGrid.getSelectedRecord().getAttributeAsBoolean(ENABLED))
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

		ListGridField enabled = new EnabledListGridField();

		ListGridField running = new RunningListGridField();

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

		ListGridField completion = new ListGridField(COMPLETION, " ", 60);
		completion.setCanFilter(false);
		completion.setCanSort(false);
		completion.setAlign(Alignment.LEFT);
		completion.setCellFormatter((value, rec, rowNum, colNum) -> value != null ? value + "%" : "");

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
						new DefaultAsyncCallback<>() {
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
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(GUITask task) {
								rec.setAttribute(STATUS, task.getStatus());
								rec.setAttribute(ENABLED, task.getScheduling().isEnabled());
								if (task.getStatus() != GUITask.STATUS_IDLE) {
									rec.setAttribute(RUNNING, true);
								} else {
									rec.setAttribute(RUNNING, false);
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
		SystemService.Instance.get().loadTasks(I18N.getLocale(), new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUITask> tasks) {
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

					updateRecord(guiTask);
				}
			}
		});
	}

	private void updateRecord(GUITask guiTask) {
		for (ListGridRecord rec : tasksGrid.getRecords()) {
			boolean found = false;
			if (rec.getAttribute("name").equals(guiTask.getName()) && guiTask.getStatus() != GUITask.STATUS_IDLE) {
				rec.setAttribute(RUNNING, true);
				rec.setAttribute(COMPLETION, guiTask.getCompletionPercentage());
				tasksGrid.refreshRow(tasksGrid.getRecordIndex(rec));
				found = true;
			} else if (rec.getAttribute("name").equals(guiTask.getName())
					&& guiTask.getStatus() == GUITask.STATUS_IDLE) {
				rec.setAttribute(RUNNING, false);
				rec.setAttribute(COMPLETION, guiTask.getCompletionPercentage());
				tasksGrid.refreshRow(tasksGrid.getRecordIndex(rec));
				found = true;
			}

			if (found)
				break;
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