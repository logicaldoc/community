package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Workflow Tools
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class WorkflowToolstrip extends ToolStrip {

	private GUIWorkflow currentWorkflow = null;

	private WorkflowDesigner designer = null;

	private ToolStripButton export = null;

	private ToolStripButton _import = null;

	private ToolStripButton save = null;

	private ToolStripButton clone = null;

	private ToolStripButton deploy = null;

	private ToolStripButton undeploy = null;

	private ToolStripButton delete = null;

	private ToolStripButton close = null;

	private ToolStripButton settings = null;

	private SelectItem workflowSelect = null;

	private PrimitivesToolstrip primitives;

	public WorkflowToolstrip(final WorkflowDesigner designer, PrimitivesToolstrip primitives) {
		super();

		this.designer = designer;
		this.currentWorkflow = designer.getWorkflow();
		this.primitives = primitives;

		setWidth100();

		workflowSelect = new SelectItem("workflow", I18N.message("workflow"));
		workflowSelect.setWidth(200);
		workflowSelect.setWrapTitle(false);
		ListGridField name = new ListGridField("name");
		workflowSelect.setValueField("name");
		workflowSelect.setDisplayField("name");
		workflowSelect.setPickListWidth(300);
		workflowSelect.setPickListFields(name);

		workflowSelect.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue() != null && !"".equals((String) event.getValue())) {
					WorkflowService.Instance.get().get((String) event.getValue(), new AsyncCallback<GUIWorkflow>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIWorkflow result) {
							if (result != null) {
								currentWorkflow = result;
								WorkflowToolstrip.this.designer.redraw(currentWorkflow);
								update();
							}
						}
					});
				}

			}
		});
		addFormItem(workflowSelect);

		ToolStripButton load = new ToolStripButton(I18N.message("load"));
		load.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord selectedRecord = workflowSelect.getSelectedRecord();
				if (selectedRecord == null)
					return;

				WorkflowService.Instance.get().get(selectedRecord.getAttributeAsString("name"),
						new AsyncCallback<GUIWorkflow>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIWorkflow result) {
								if (result != null) {
									currentWorkflow = result;
									WorkflowToolstrip.this.designer.redraw(result);
									update();
								}
							}
						});
			}
		});
		addButton(load);

		ToolStripButton newTemplate = new ToolStripButton(I18N.message("new"));
		newTemplate.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				event.cancel();

				FormItem workflowName = ItemFactory.newSimpleTextItem("workflowName", "workflowname", null);
				workflowName.setRequired(true);
				LD.askforValue(I18N.message("newwftemplate"), I18N.message("workflowname"), null, workflowName,
						new ValueCallback() {

							@Override
							public void execute(String value) {
								if (value != null && !value.trim().isEmpty()) {
									GUIWorkflow newWF = new GUIWorkflow();
									newWF.setName(value);
									AdminScreen.get().setContent(new WorkflowDesigner(newWF));
								}
							}
						});
			}
		});
		addButton(newTemplate);

		settings = new ToolStripButton(I18N.message("settings"));
		settings.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				WorkflowSettings settings = new WorkflowSettings(designer.getWorkflow());
				settings.show();
			}
		});
		addButton(settings);

		save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		addButton(save);

		deploy = new ToolStripButton(I18N.message("deploy"));
		deploy.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				WorkflowToolstrip.this.designer.saveModel();
				currentWorkflow = WorkflowToolstrip.this.designer.getWorkflow();

				boolean taskFound = false;
				if (currentWorkflow.getStates() != null && currentWorkflow.getStates().length > 0)
					for (GUIWFState state : currentWorkflow.getStates()) {
						if (state.getType() == GUIWFState.TYPE_TASK) {
							taskFound = true;
							break;
						}
					}

				boolean transitionErrorFound = false;
				boolean stateWithoutAssigneeFound = false;
				if (currentWorkflow.getStates() != null && currentWorkflow.getStates().length > 0) {
					for (GUIWFState state : currentWorkflow.getStates()) {
						if (state.getType() == GUIWFState.TYPE_TASK
								&& (state.getParticipants() == null || state.getParticipants().length == 0)) {
							stateWithoutAssigneeFound = true;
							break;
						}
						if (transitionErrorFound) {
							break;
						}
						if (state.getType() != GUIWFState.TYPE_END) {
							if (state.getTransitions() == null) {
								transitionErrorFound = true;
								break;
							}
							for (GUITransition transition : state.getTransitions()) {
								if (transition.getTargetState() == null
										|| (transition.getTargetState() != null && transition.getTargetState()
												.getType() == GUIWFState.TYPE_UNDEFINED)) {
									transitionErrorFound = true;
									break;
								}
							}
						}
					}
				}

				if (!taskFound)
					SC.warn(I18N.message("workflowtaskatleast"));
				else if (stateWithoutAssigneeFound)
					SC.warn(I18N.message("workflowtaskparticipantatleast"));
				else if (transitionErrorFound)
					SC.warn(I18N.message("workflowtransitiontarget"));
				else {
					ContactingServer.get().show();
					WorkflowService.Instance.get().deploy(currentWorkflow, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							ContactingServer.get().hide();
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							ContactingServer.get().hide();
							SC.say(I18N.message("workflowdeployed", currentWorkflow.getName()));
							update();
						}
					});
				}
			}
		});
		addButton(deploy);

		undeploy = new ToolStripButton(I18N.message("undeploy"));
		undeploy.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				WorkflowToolstrip.this.designer.saveModel();
				currentWorkflow = WorkflowToolstrip.this.designer.getWorkflow();
				if (currentWorkflow == null || currentWorkflow.getName() == null)
					return;

				LD.ask(I18N.message("undeploy"), I18N.message("undeploywarn"), new BooleanCallback() {

					@Override
					public void execute(Boolean value) {
						if (value.booleanValue())
							WorkflowService.Instance.get().undeploy(currentWorkflow.getName(),
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void result) {
											SC.say(I18N.message("workflowundeployed", currentWorkflow.getName()));
											update();
										}
									});
					}
				});
			}
		});
		addButton(undeploy);

		delete = new ToolStripButton(I18N.message("ddelete"));
		delete.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							WorkflowService.Instance.get().delete(currentWorkflow.getName(), new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									currentWorkflow = new GUIWorkflow();
									AdminScreen.get().setContent(new WorkflowDesigner(currentWorkflow));
									update();
								}
							});
						}
					}
				});
			}
		});
		addButton(delete);
		addSeparator();

		clone = new ToolStripButton(I18N.message("clone"));
		clone.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				// Ask for new name
				LD.askForValue(I18N.message("clone"), I18N.message("newname"), "", new ValueCallback() {
					@Override
					public void execute(String value) {
						if (value == null || "".equals(value.trim()))
							return;
						// Set the new name in the designer, then
						// request a save
						currentWorkflow.setId(null);
						currentWorkflow.setName(value.trim());
						onSave();
					}
				});
			}
		});
		addButton(clone);

		_import = new ToolStripButton(I18N.message("iimport"));
		_import.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				WorkflowUploader uploader = new WorkflowUploader(WorkflowToolstrip.this.designer);
				uploader.show();
				update();
			}
		});
		addButton(_import);

		export = new ToolStripButton(I18N.message("eexport"));
		export.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				WindowUtils.openUrl(Util.contextPath()
						+ "download?pluginId=logicaldoc-workflow&resourcePath=templates/" + currentWorkflow.getId()
						+ ".ldpm");
			}
		});
		addButton(export);
		addSeparator();

		close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				try {
					currentWorkflow = new GUIWorkflow();
					AdminScreen.get().setContent(new WorkflowDesigner(currentWorkflow));
					update();
				} catch (Throwable t) {

				}
			}
		});
		addButton(close);

		addFill();

		update();
	}

	public WorkflowDesigner getDesigner() {
		return designer;
	}

	private void update() {
		export.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| "0".equals(currentWorkflow.getId()));
		_import.setDisabled(currentWorkflow == null || currentWorkflow.getName() == null
				|| currentWorkflow.getName().isEmpty());
		save.setDisabled(currentWorkflow == null || currentWorkflow.getName() == null
				|| currentWorkflow.getName().isEmpty());
		clone.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| "0".equals(currentWorkflow.getId()));
		deploy.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| "0".equals(currentWorkflow.getId()));
		undeploy.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| "0".equals(currentWorkflow.getId()));
		delete.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| "0".equals(currentWorkflow.getId()));
		settings.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| "0".equals(currentWorkflow.getId()));
		close.setDisabled(currentWorkflow == null);

		workflowSelect.setOptionDataSource(new WorkflowsDS(false, false));

		if (currentWorkflow != null && !currentWorkflow.getName().trim().isEmpty()) {
			workflowSelect.setValue(currentWorkflow.getName());
		} else
			workflowSelect.setValue(I18N.message("workflowselect") + "...");
		workflowSelect.redraw();

		primitives.update();
	}

	private void onSave() {
		try {
			if (!designer.saveModel())
				return;
		} catch (Throwable t) {
		}

		currentWorkflow = designer.getWorkflow();

		ContactingServer.get().show();
		WorkflowService.Instance.get().save(currentWorkflow, new AsyncCallback<GUIWorkflow>() {
			@Override
			public void onFailure(Throwable caught) {
				ContactingServer.get().hide();
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIWorkflow result) {
				ContactingServer.get().hide();
				if (result == null) {
					SC.warn(I18N.message("workflowalreadyexist"));
				} else {
					if (currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId())) {
						currentWorkflow = result;
						designer.redraw(currentWorkflow);
					} else
						currentWorkflow = result;
				}
				update();
			}
		});
	}
}