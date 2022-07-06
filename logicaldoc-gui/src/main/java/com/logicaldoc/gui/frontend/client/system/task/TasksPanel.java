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
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the list of scheduled tasks
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TasksPanel extends AdminPanel {

	private Layout results = new VLayout();

	private Layout details;

	private ListGrid list;

	private Timer timer;

	private Canvas detailPanel;

	private Map<String, Progressbar> progresses = new HashMap<String, Progressbar>();

	public TasksPanel() {
		super("scheduledtasks");
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem taskExecution = new MenuItem();
		taskExecution.setTitle(I18N.message("execute"));
		taskExecution.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SystemService.Instance.get().getTaskByName(list.getSelectedRecord().getAttributeAsString("name"),
						I18N.getLocale(), new AsyncCallback<GUITask>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUITask task) {
								final GUITask currentTask = task;
								SystemService.Instance.get().startTask(currentTask.getName(),
										new AsyncCallback<Boolean>() {
											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Boolean result) {
												final ListGridRecord record = list.getSelectedRecord();
												record.setAttribute("status", GUITask.STATUS_RUNNING);
												record.setAttribute("runningIcon", "running_task");
												Date now = new Date();
												record.setAttribute("lastStart", now);
												record.setAttribute("nextStart", new Date(now.getTime()
														+ (currentTask.getScheduling().getInterval() * 1000)));
												list.refreshRow(list.getRecordIndex(record));
											}
										});
							}
						});
			}
		});

		if (GUITask.STATUS_IDLE != list.getSelectedRecord().getAttributeAsInt("status")
				|| !list.getSelectedRecord().getAttributeAsBoolean("eenabled"))
			taskExecution.setEnabled(false);

		MenuItem taskStop = new MenuItem();
		taskStop.setTitle(I18N.message("stop"));
		taskStop.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SystemService.Instance.get().stopTask(list.getSelectedRecord().getAttributeAsString("name"),
						new AsyncCallback<Boolean>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Boolean result) {
								list.getSelectedRecord().setAttribute("status", GUITask.STATUS_STOPPING);
								list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
							}
						});

			}
		});

		if (GUITask.STATUS_RUNNING != list.getSelectedRecord().getAttributeAsInt("status")
				|| !list.getSelectedRecord().getAttributeAsBoolean("eenabled"))
			taskStop.setEnabled(false);

		MenuItem enableTask = new MenuItem();
		enableTask.setTitle(I18N.message("enable"));
		enableTask.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SystemService.Instance.get().enableTask(list.getSelectedRecord().getAttributeAsString("name"),
						new AsyncCallback<Boolean>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Boolean result) {
								list.getSelectedRecord().setAttribute("enabledIcon", "bullet_green");
								list.getSelectedRecord().setAttribute("eenabled", true);
								list.getSelectedRecord().setAttribute("runningIcon", "idle_task");
								list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
							}
						});

			}
		});

		if (list.getSelectedRecord().getAttributeAsBoolean("eenabled"))
			enableTask.setEnabled(false);

		MenuItem disableTask = new MenuItem();
		disableTask.setTitle(I18N.message("disable"));
		disableTask.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SystemService.Instance.get().disableTask(list.getSelectedRecord().getAttributeAsString("name"),
						new AsyncCallback<Boolean>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Boolean result) {
								list.getSelectedRecord().setAttribute("enabledIcon", "bullet_red");
								list.getSelectedRecord().setAttribute("eenabled", false);
								list.getSelectedRecord().setAttribute("runningIcon", "idle_task");
								list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
							}
						});

			}
		});

		if (!list.getSelectedRecord().getAttributeAsBoolean("eenabled")
				|| list.getSelectedRecord().getAttributeAsInt("status") != GUITask.STATUS_IDLE)
			disableTask.setEnabled(false);

		contextMenu.setItems(taskExecution, taskStop, enableTask, disableTask);
		contextMenu.showContextMenu();
	}

	private void prepareTasksGrid() {
		list = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord record, Integer colNum) {
				String fieldName = this.getFieldName(colNum);
				if (fieldName.equals("progressbar")) {
					Progressbar prgBar = new Progressbar();
					prgBar.setLength(100);
					prgBar.setBreadth(15);
					progresses.put(record.getAttribute("name"), prgBar);

					return prgBar;
				} else {
					return null;
				}
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));

		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setAutoFetchData(true);

		ListGridField enabled = new ListGridField("enabledIcon", " ", 30);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".png");
		enabled.setCanFilter(false);

		ListGridField running = new ListGridField("runningIcon", " ", 30);
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

		ListGridField scheduling = new ListGridField("scheduling", I18N.message("scheduling"), 130);
		scheduling.setCanFilter(false);
		scheduling.setAlign(Alignment.CENTER);
		scheduling.setCanSort(false);

		ListGridField progressbar = new ListGridField("progressbar", I18N.message("progress"), 110);
		progressbar.setCanFilter(false);
		progressbar.setCanSort(false);
		progressbar.setAlign(Alignment.LEFT);
		progressbar.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return "";
			}
		});

		ListGridField completion = new ListGridField("completion", " ", 35);
		completion.setCanFilter(false);
		completion.setCanSort(false);
		completion.setAlign(Alignment.LEFT);
		completion.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if (value != null)
					return value + "%";
				else
					return "";
			}
		});

		list.setWidth100();
		list.setHeight100();
		list.setFields(enabled, running, label, description, lastStart, nextStart, scheduling, progressbar, completion);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setFilterOnKeypress(true);
		list.setDataSource(new TasksDS());

		results.addMember(list, 1);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				event.cancel();
				if (!Session.get().isDefaultTenant())
					return;

				final ListGridRecord record = list.getSelectedRecord();
				if (record != null)
					SystemService.Instance.get().getTaskByName(record.getAttribute("name"), I18N.getLocale(),
							new AsyncCallback<GUITask>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUITask task) {
									record.setAttribute("status", task.getStatus());
									record.setAttribute("eenabled", task.getScheduling().isEnabled());
									if (task.getStatus() != GUITask.STATUS_IDLE) {
										record.setAttribute("runningIcon", "running_task");
									} else {
										record.setAttribute("runningIcon", "idle_task");
									}
									list.refreshRow(list.getRecordIndex(record));
									showContextMenu();
								}
							});
			}
		});

		list.addSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				ListGridRecord record = list.getSelectedRecord();
				if (record != null)
					SystemService.Instance.get().getTaskByName(record.getAttribute("name"), I18N.getLocale(),
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
			}
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

							for (ListGridRecord record : list.getRecords()) {
								if (record.getAttribute("name").equals(guiTask.getName())
										&& guiTask.getStatus() != GUITask.STATUS_IDLE) {
									record.setAttribute("runningIcon", "running_task");
									record.setAttribute("completion", guiTask.getCompletionPercentage());
									list.refreshRow(list.getRecordIndex(record));
									break;
								} else if (record.getAttribute("name").equals(guiTask.getName())
										&& guiTask.getStatus() == GUITask.STATUS_IDLE) {
									record.setAttribute("runningIcon", "idle_task");
									record.setAttribute("completion", guiTask.getCompletionPercentage());
									list.refreshRow(list.getRecordIndex(record));
									break;
								}
							}
						}
					}
				});
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
		return list;
	}

	/**
	 * Updates the selected record with the new task data
	 * 
	 * @param task the task to update
	 */
	public void updateSelectedRecord(GUITask task) {
		list.getSelectedRecord().setAttribute("scheduling", task.getSchedulingLabel());
		list.refreshRow(list.getRecordIndex(list.getSelectedRecord()));
	}
}