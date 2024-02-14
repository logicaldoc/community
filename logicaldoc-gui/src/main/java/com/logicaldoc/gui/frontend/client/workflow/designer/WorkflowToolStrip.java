package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.Arrays;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.data.WorkflowsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.PrintUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
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
public class WorkflowToolStrip extends ToolStrip {

	private static final String DEPLOYED = "deployed";

	private static final String VERSION = "version";

	private GUIWorkflow currentWorkflow = null;

	private WorkflowDesigner designer = null;

	private ToolStripButton export = null;

	private ToolStripButton importButton = null;

	private ToolStripButton save = null;

	private ToolStripButton clone = null;

	private ToolStripButton deploy = null;

	private ToolStripButton undeploy = null;

	private ToolStripButton delete = null;

	private ToolStripButton print = null;

	private ToolStripButton close = null;

	private ToolStripButton load = null;

	private ToolStripButton settings = null;

	private ToolStripButton security = null;

	private SelectItem workflowSelector = null;

	private SelectItem versionSelector = null;

	private PrimitivesToolstrip primitives;

	public WorkflowToolStrip(final WorkflowDesigner designer, PrimitivesToolstrip primitives) {
		super();

		this.designer = designer;
		this.currentWorkflow = designer.getWorkflow();
		this.primitives = primitives;

		setWidth100();

		addWorkflowSelector();

		addVersionSelector();

		addSeparator();

		addLoadButton();

		addNewWorkflowButton();

		addSettingsButton(designer);

		addSecurityButton(designer);

		addSave();

		addDeployButton();

		addUndeployButton();

		addDeleteButton();

		addSeparator();

		addCloneButton();

		addImportButton();

		addExportButton();

		print = new ToolStripButton(I18N.message("print"));
		print.addClickHandler(
				event -> PrintUtil.printScreenShot(designer.getDrawingPanel().getID(), I18N.message("workflow") + " - "
						+ designer.getWorkflow().getName() + " v" + designer.getWorkflow().getVersion()));
		addButton(print);

		addSeparator();

		close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> {
			try {
				currentWorkflow = new GUIWorkflow();
				AdminScreen.get().setContent(new WorkflowDesigner(currentWorkflow));
				update();
			} catch (Exception t) {
				// Nothing to do
			}
		});
		addButton(close);

		addFill();

		update();
	}

	private void addExportButton() {
		export = new ToolStripButton(I18N.message("eexport"));
		export.addClickHandler(event -> Util
				.download(Util.contextPath() + "workflow/controller?command=export&wfId=" + currentWorkflow.getId()));
		addButton(export);
	}

	private void addImportButton() {
		importButton = new ToolStripButton(I18N.message("iimport"));
		importButton.addClickHandler(event -> {
			new WorkflowUploader(WorkflowToolStrip.this.designer).show();
			update();
		});
		addButton(importButton);
	}

	private void addCloneButton() {
		clone = new ToolStripButton(I18N.message("clone"));
		clone.addClickHandler(event ->
		// Ask for new name
		LD.askForValue(I18N.message("clone"), I18N.message("newname"), "",
				ItemFactory.newSimpleTextItem("name", "newname", ""), null, (String newName) -> {
					if (newName == null || "".equals(newName.trim()))
						return;

					// Set the new name in the designer, then
					// request a save
					currentWorkflow.setId(null);
					currentWorkflow.setName(newName.trim());
					currentWorkflow.setLabel(newName.trim());
					onSave();
					updateVersionSelect();
				}));
		addButton(clone);
	}

	private void addDeleteButton() {
		delete = new ToolStripButton(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				WorkflowService.Instance.get().delete(currentWorkflow.getName(), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						currentWorkflow = new GUIWorkflow();
						AdminScreen.get().setContent(new WorkflowDesigner(currentWorkflow));
						update();
					}
				});
			}
		}));
		addButton(delete);
	}

	private void addUndeployButton() {
		undeploy = new ToolStripButton(I18N.message("undeploy"));
		undeploy.addClickHandler(event -> {
			WorkflowToolStrip.this.designer.saveModel();
			currentWorkflow = WorkflowToolStrip.this.designer.getWorkflow();
			if (currentWorkflow == null || currentWorkflow.getName() == null)
				return;

			LD.ask(I18N.message("undeploy"), I18N.message("undeploywarn"), (Boolean yes) -> {
				if (Boolean.TRUE.equals(yes))
					WorkflowService.Instance.get().undeploy(currentWorkflow.getName(), new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							GuiLog.info(I18N.message("workflowundeployed", currentWorkflow.getName()));
							update();
							reload(currentWorkflow.getName());
							workflowSelector.getSelectedRecord().setAttribute(DEPLOYED, false);
						}
					});
			});
		});
		addButton(undeploy);
	}

	private void addDeployButton() {
		deploy = new ToolStripButton(I18N.message("deploy"));
		deploy.addClickHandler(event -> onDeploy());
		addButton(deploy);
	}

	private void onDeploy() {
		WorkflowToolStrip.this.designer.saveModel();
		currentWorkflow = WorkflowToolStrip.this.designer.getWorkflow();

		boolean taskFound = checkTaskPresence();

		boolean taskWithoutParticipantsFound = checkTaskWithoutParticipants();

		boolean taskWithoutTransitionsFound = checkTaskWithoutTransitions();

		if (!taskFound)
			SC.warn(I18N.message("workflowtaskatleast"));
		else if (taskWithoutParticipantsFound)
			SC.warn(I18N.message("workflowtaskparticipantatleast"));
		else if (taskWithoutTransitionsFound)
			SC.warn(I18N.message("workflowtransitiontarget"));
		else {
			LD.contactingServer();
			WorkflowService.Instance.get().deploy(currentWorkflow, new AsyncCallback<GUIWorkflow>() {
				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIWorkflow result) {
					LD.clearPrompt();
					GuiLog.info(I18N.message("workflowdeployed", currentWorkflow.getName()));
					currentWorkflow = result;
					reload(currentWorkflow.getName());
					workflowSelector.getSelectedRecord().setAttribute(DEPLOYED, true);
				}
			});
		}
	}

	private boolean checkTaskWithoutTransitions() {
		boolean transitionErrorFound = false;

		for (GUIWFState state : currentWorkflow.getStates()) {
			if (state.getType() != GUIWFState.TYPE_END) {
				if (state.getTransitions() == null) {
					transitionErrorFound = true;
					break;
				}
				for (GUITransition transition : state.getTransitions()) {
					if (transition.getTargetState() == null || (transition.getTargetState() != null
							&& transition.getTargetState().getType() == GUIWFState.TYPE_UNDEFINED)) {
						transitionErrorFound = true;
						break;
					}
				}
			}
		}

		return transitionErrorFound;
	}

	private boolean checkTaskWithoutParticipants() {
		return currentWorkflow.getStates().stream()
				.anyMatch(state -> state.getType() == GUIWFState.TYPE_TASK && state.getParticipants().isEmpty());
	}

	private boolean checkTaskPresence() {
		return currentWorkflow.getStates().stream().anyMatch(state -> state.getType() == GUIWFState.TYPE_TASK);
	}

	private void addSave() {
		save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());
		addButton(save);
	}

	private void addSecurityButton(final WorkflowDesigner designer) {
		security = new ToolStripButton(I18N.message("security"));
		security.addClickHandler(event -> new WorkflowSecurity(designer.getWorkflow()).show());
		addButton(security);
	}

	private void addSettingsButton(final WorkflowDesigner designer) {
		settings = new ToolStripButton(I18N.message("settings"));
		settings.addClickHandler(event -> new WorkflowSettings(designer.getWorkflow()).show());
		addButton(settings);
	}

	private void addNewWorkflowButton() {
		ToolStripButton newWorkflow = new ToolStripButton(I18N.message("new"));
		newWorkflow.addClickHandler(event -> {
			event.cancel();

			FormItem workflowName = ItemFactory.newSimpleTextItem("workflowName", null);
			workflowName.setRequired(true);
			LD.askForValue(I18N.message("newwftemplate"), I18N.message("workflowname"), null, workflowName, value -> {
				if (value != null && !value.trim().isEmpty()) {
					GUIWorkflow newWF = new GUIWorkflow();
					newWF.setName(value);
					newWF.setPermissions(Arrays.asList(GUIAccessControlEntry.PERMISSION_WRITE));
					newWF.setLatestVersion(true);

					AdminScreen.get().setContent(new WorkflowDesigner(newWF));
				}
			});
		});
		addButton(newWorkflow);
	}

	private void addLoadButton() {
		load = new ToolStripButton(I18N.message("load"));
		load.addClickHandler(event -> {
			ListGridRecord selectedRecord = workflowSelector.getSelectedRecord();
			if (selectedRecord == null)
				return;

			reload(selectedRecord.getAttributeAsString("name"));
		});
		addButton(load);
	}

	private void addVersionSelector() {
		versionSelector = new SelectItem(VERSION, I18N.message(VERSION));
		versionSelector.setWidth(60);
		versionSelector.setWrapTitle(false);
		ListGridField version = new ListGridField(VERSION);
		ListGridField date = new ListGridField("date");
		ListGridField deployed = new ListGridField(DEPLOYED);
		versionSelector.setValueField(VERSION);
		versionSelector.setDisplayField(VERSION);
		versionSelector.setPickListWidth(200);
		versionSelector.setPickListFields(version, date, deployed);
		versionSelector.addChangedHandler(event ->

		WorkflowService.Instance.get().get(currentWorkflow.getName(), (Integer) event.getValue(),
				new AsyncCallback<GUIWorkflow>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIWorkflow result) {
						if (result != null) {
							currentWorkflow = result;
							WorkflowToolStrip.this.designer.redraw(currentWorkflow);
							update();
						}
					}
				}));
		addFormItem(versionSelector);
	}

	private void addWorkflowSelector() {
		workflowSelector = ItemFactory.newWorkflowSelectorForAdministration(Session.get().getUser().getId());
		workflowSelector.addChangedHandler(event -> {
			if (event.getValue() != null && !"".equals(event.getValue())) {
				WorkflowService.Instance.get().get(workflowSelector.getSelectedRecord().getAttributeAsString("name"),
						null, new AsyncCallback<GUIWorkflow>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIWorkflow result) {
								if (result != null) {
									currentWorkflow = result;
									WorkflowToolStrip.this.designer.redraw(currentWorkflow);
									updateVersionSelect();
									update();
								}
							}
						});
			}
		});
		addFormItem(workflowSelector);
	}

	public WorkflowDesigner getDesigner() {
		return designer;
	}

	private void update() {
		export.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId()));
		importButton.setDisabled(
				currentWorkflow == null || currentWorkflow.getName() == null || currentWorkflow.getName().isEmpty()
						|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		save.setDisabled(
				currentWorkflow == null || currentWorkflow.getName() == null || currentWorkflow.getName().isEmpty()
						|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		clone.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId()));
		deploy.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId())
						|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		undeploy.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId())
						|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		delete.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId())
						|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		settings.setDisabled(currentWorkflow == null || currentWorkflow.getId() == null
				|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		security.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId())
						|| !currentWorkflow.isLatestVersion() || !currentWorkflow.isWrite());
		print.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId()));
		load.setDisabled(
				currentWorkflow == null || currentWorkflow.getId() == null || "0".equals(currentWorkflow.getId()));
		close.setDisabled(currentWorkflow == null);

		primitives.update();
	}

	private void onSave() {
		try {
			if (!designer.saveModel())
				return;
		} catch (Exception t) {
			// Nothing to do
		}

		currentWorkflow = designer.getWorkflow();

		LD.contactingServer();
		WorkflowService.Instance.get().save(currentWorkflow, new AsyncCallback<GUIWorkflow>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIWorkflow result) {
				LD.clearPrompt();
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
				reload(currentWorkflow.getName());

				workflowSelector.setOptionDataSource(new WorkflowsDS(false, false, Session.get().getUser().getId()));
				workflowSelector.setValue(currentWorkflow.getId());
			}
		});
	}

	protected void updateVersionSelect() {
		versionSelector.setOptionDataSource(
				new WorkflowsDS(currentWorkflow.getName(), false, false, false, Session.get().getUser().getId()));
		versionSelector.setValue(currentWorkflow.getVersion());
	}

	protected void reload(String workflowName) {
		WorkflowService.Instance.get().get(workflowName, null, new AsyncCallback<GUIWorkflow>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIWorkflow result) {
				if (result != null) {
					currentWorkflow = result;
					WorkflowToolStrip.this.designer.redraw(result);
					updateVersionSelect();
					update();
				}
			}
		});
	}
}