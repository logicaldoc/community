package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.FolderObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.gui.frontend.client.workflow.WorkflowTriggersPanel;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all the folder details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderDetailsPanel extends VLayout implements FolderObserver {

	private GUIFolder folder;

	private Layout propertiesTabPanel;

	private Layout extendedPropertiesTabPanel;

	private Layout securityTabPanel;

	private Layout historyTabPanel;

	private Layout workflowsTabPanel;

	private Layout subscriptionsTabPanel;

	private Layout quotaTabPanel;

	private Layout aliasesTabPanel;

	private Layout automationTabPanel;

	private Layout interfaceTabPanel;

	private Layout captureTabPanel;

	private FolderStandardPropertiesPanel propertiesPanel;

	private FolderExtendedPropertiesPanel extendedPropertiesPanel;

	private FolderSecurityPanel securityPanel;

	private FolderHistoryPanel historyPanel;

	private AliasesPanel aliasesPanel;

	private WorkflowTriggersPanel workflowsPanel;

	private FolderSubscriptionsPanel subscriptionsPanel;

	private FolderQuotaPanel quotaPanel;

	private FolderAutomationPanel automationPanel;

	private FolderInterfacePanel interfacePanel;

	private FolderCapturePanel ocrPanel;

	private EditingTabSet tabSet;

	private Tab quotaTab = null;

	private Tab automationTab = null;

	private Tab interfaceTab = null;

	private Tab captureTab = null;

	public FolderDetailsPanel(GUIFolder folder) {
		super();

		this.folder = folder;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		prepareTabSet();

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		Tab extendedPropertiesTab = new Tab(I18N.message("propertiesext"));
		extendedPropertiesTabPanel = new HLayout();
		extendedPropertiesTabPanel.setHeight100();
		extendedPropertiesTab.setPane(extendedPropertiesTabPanel);
		tabSet.addTab(extendedPropertiesTab);

		Tab securityTab = new Tab(I18N.message("security"));
		securityTabPanel = new HLayout();
		securityTabPanel.setWidth100();
		securityTabPanel.setHeight100();
		securityTab.setPane(securityTabPanel);
		if (folder.hasPermission(Constants.PERMISSION_SECURITY))
			tabSet.addTab(securityTab);

		Tab historyTab = new Tab(I18N.message("history"));
		historyTabPanel = new HLayout();
		historyTabPanel.setWidth100();
		historyTabPanel.setHeight100();
		historyTab.setPane(historyTabPanel);
		if (Menu.enabled(Menu.HISTORY))
			tabSet.addTab(historyTab);

		prepareWorkflowTab(folder);

		prepareSubscriptionsTab(folder);

		prepareQuotaTab(folder);

		Tab aliasesTab = new Tab(I18N.message("aliases"));
		aliasesTabPanel = new HLayout();
		aliasesTabPanel.setWidth100();
		aliasesTabPanel.setHeight100();
		aliasesTab.setPane(aliasesTabPanel);
		if (Menu.enabled(Menu.ALIASES) && folder.getFoldRef() == null)
			tabSet.addTab(aliasesTab);

		automationTab = new Tab(I18N.message("automation"));
		if (folder.hasPermission(Constants.PERMISSION_AUTOMATION) && Feature.visible(Feature.AUTOMATION)) {
			if (Feature.enabled(Feature.AUTOMATION)) {
				automationTabPanel = new HLayout();
				automationTabPanel.setWidth100();
				automationTabPanel.setHeight100();
			} else {
				automationTabPanel = new FeatureDisabled();
			}
			automationTab.setPane(automationTabPanel);
			tabSet.addTab(automationTab);
		}

		if (Menu.enabled(Menu.FOLDER_INTERFACE)) {
			interfaceTab = new Tab(I18N.message("userinterface"));
			interfaceTabPanel = new HLayout();
			interfaceTabPanel.setWidth100();
			interfaceTabPanel.setHeight100();
			interfaceTab.setPane(interfaceTabPanel);
			tabSet.addTab(interfaceTab);
		}

		if (Menu.enabled(Menu.CAPTURE)) {
			captureTab = new Tab(I18N.message("capture"));
			captureTabPanel = new HLayout();
			captureTabPanel.setWidth100();
			captureTabPanel.setHeight100();
			captureTab.setPane(captureTabPanel);
			tabSet.addTab(captureTab);
		}

		addMember(tabSet);
		refresh();
	}

	private void prepareQuotaTab(GUIFolder folder) {
		try {
			quotaTab = new Tab(I18N.message("quota"));
			if (folder.isWorkspace() && folder.hasPermission(Constants.PERMISSION_WRITE)
					&& Feature.visible(Feature.QUOTAS)) {
				if (Feature.enabled(Feature.QUOTAS)) {
					quotaTabPanel = new HLayout();
					quotaTabPanel.setWidth100();
					quotaTabPanel.setHeight100();
				} else {
					quotaTabPanel = new FeatureDisabled();
				}
				quotaTab.setPane(quotaTabPanel);
				tabSet.addTab(quotaTab);
			}
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareSubscriptionsTab(GUIFolder folder) {
		Tab subscriptionsTab = new Tab(I18N.message("subscriptions"));
		if (folder.hasPermission(Constants.PERMISSION_SUBSCRIPTION) && Feature.visible(Feature.AUDIT)) {
			if (Feature.enabled(Feature.AUDIT)) {
				subscriptionsTabPanel = new HLayout();
				subscriptionsTabPanel.setWidth100();
				subscriptionsTabPanel.setHeight100();
			} else {
				subscriptionsTabPanel = new FeatureDisabled();
			}
			subscriptionsTab.setPane(subscriptionsTabPanel);
			tabSet.addTab(subscriptionsTab);
		}
	}

	private void prepareWorkflowTab(GUIFolder folder) {
		Tab workflowTab = new Tab(I18N.message("workflow"));
		if (folder.hasPermission(Constants.PERMISSION_WORKFLOW) && Feature.visible(Feature.WORKFLOW)) {
			if (Feature.enabled(Feature.WORKFLOW)) {
				workflowsTabPanel = new HLayout();
				workflowsTabPanel.setWidth100();
				workflowsTabPanel.setHeight100();
			} else {
				workflowsTabPanel = new FeatureDisabled();
			}
			workflowTab.setPane(workflowsTabPanel);
			tabSet.addTab(workflowTab);
		}
	}

	private void prepareTabSet() {
		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> FolderService.Instance.get()
				.getFolder(getFolder().getId(), false, false, false, new AsyncCallback<GUIFolder>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder folder) {
						folder.setPathExtended(folder.getPathExtended() != null ? folder.getPathExtended()
								: FolderNavigator.get().getPath(folder.getId()));
						setFolder(folder);
						tabSet.hideSave();
					}
				}));
	}

	private void refresh() {
		try {
			disableSave();

			ChangedHandler changeHandler = changeEvent -> onModified();

			/*
			 * Prepare the standard properties tab
			 */
			prepareStandardPropertiesTab(changeHandler);

			/*
			 * Prepare the extended properties tab
			 */
			prepareExtendedPropertiesTab(changeHandler);

			/*
			 * Prepare the security properties tab
			 */
			prepareSecurityTab();

			/*
			 * Prepare the aliases tab
			 */
			prepareAliasesTab();

			/*
			 * Prepare the history tab
			 */
			prepareHistoryTab();

			addWorkflowTab();

			addSubscriptionsTab();

			addQuotaTab(changeHandler);

			addAutomationTab();

			addInterfaceTab(changeHandler);

			addCapturePanel(changeHandler);
		} catch (Exception e) {
			GuiLog.error(e.getMessage(), null, e);
		}
	}

	private void addCapturePanel(ChangedHandler changeHandler) {
		if (Menu.enabled(Menu.CAPTURE)) {
			/*
			 * Prepare the OCR tab
			 */
			if (ocrPanel != null) {
				ocrPanel.destroy();
				captureTabPanel.removeMember(ocrPanel);
			}
			ocrPanel = new FolderCapturePanel(folder, changeHandler);
			captureTabPanel.addMember(ocrPanel);
		}
	}

	private void addInterfaceTab(ChangedHandler changeHandler) {
		if (Menu.enabled(Menu.FOLDER_INTERFACE)) {
			/*
			 * Prepare the User Interface tab
			 */
			if (interfacePanel != null) {
				interfacePanel.destroy();
				interfaceTabPanel.removeMember(interfacePanel);
			}
			interfacePanel = new FolderInterfacePanel(folder, changeHandler);
			interfaceTabPanel.addMember(interfacePanel);
		}
	}

	private void addAutomationTab() {
		if (Feature.enabled(Feature.AUTOMATION) && folder.hasPermission(Constants.PERMISSION_AUTOMATION)) {
			/*
			 * Prepare the subscriptions tab
			 */
			if (automationPanel != null) {
				automationPanel.destroy();
				automationTabPanel.removeMember(automationPanel);
			}

			automationPanel = new FolderAutomationPanel(folder);
			automationTabPanel.addMember(automationPanel);
		}
	}

	private void addQuotaTab(ChangedHandler changeHandler) {
		if (Feature.enabled(Feature.QUOTAS) && folder.isWorkspace()
				&& folder.hasPermission(Constants.PERMISSION_WRITE)) {
			/*
			 * Prepare the subscriptions tab
			 */
			if (quotaPanel != null) {
				quotaPanel.destroy();
				quotaTabPanel.removeMember(quotaPanel);
			}

			quotaPanel = new FolderQuotaPanel(folder, changeHandler);
			quotaTabPanel.addMember(quotaPanel);
		}
	}

	private void addSubscriptionsTab() {
		if (Feature.enabled(Feature.AUDIT) && folder.hasPermission(Constants.PERMISSION_SUBSCRIPTION)) {
			/*
			 * Prepare the subscriptions tab
			 */
			if (subscriptionsPanel != null) {
				subscriptionsPanel.destroy();
				subscriptionsTabPanel.removeMember(subscriptionsPanel);
			}

			subscriptionsPanel = new FolderSubscriptionsPanel(folder);
			subscriptionsTabPanel.addMember(subscriptionsPanel);
		}
	}

	private void addWorkflowTab() {
		if (Feature.enabled(Feature.WORKFLOW) && folder.hasPermission(Constants.PERMISSION_WORKFLOW)) {
			/*
			 * Prepare the workflow tab
			 */
			if (workflowsPanel != null) {
				workflowsPanel.destroy();
				workflowsTabPanel.removeMember(workflowsPanel);
			}

			workflowsPanel = new WorkflowTriggersPanel(folder);
			workflowsTabPanel.addMember(workflowsPanel);
		}
	}

	private void prepareHistoryTab() {
		if (historyPanel != null) {
			historyPanel.destroy();
			historyTabPanel.removeMember(historyPanel);
		}
		historyPanel = new FolderHistoryPanel(folder);
		historyTabPanel.addMember(historyPanel);
	}

	private void prepareAliasesTab() {
		if (aliasesPanel != null) {
			aliasesPanel.destroy();
			aliasesTabPanel.removeMember(aliasesPanel);
		}
		aliasesPanel = new AliasesPanel(folder);
		aliasesTabPanel.addMember(aliasesPanel);
	}

	private void prepareSecurityTab() {
		if (securityPanel != null) {
			securityPanel.destroy();
			securityTabPanel.removeMember(securityPanel);
		}
		securityPanel = new FolderSecurityPanel(folder);
		securityTabPanel.addMember(securityPanel);
	}

	private void prepareExtendedPropertiesTab(ChangedHandler changeHandler) {
		if (extendedPropertiesPanel != null) {
			extendedPropertiesPanel.destroy();
			extendedPropertiesTabPanel.removeMember(extendedPropertiesPanel);
		}

		ChangedHandler templateChangedHandler = (ChangedEvent templateChangeEvent) -> {
			folder.setOcrTemplateId(null);
			folder.setBarcodeTemplateId(null);
			ocrPanel.refresh(folder.getTemplateId());
		};
		extendedPropertiesPanel = new FolderExtendedPropertiesPanel(folder, changeHandler, templateChangedHandler);
		if (Feature.enabled(Feature.TEMPLATE))
			extendedPropertiesTabPanel.addMember(extendedPropertiesPanel);
	}

	private void prepareStandardPropertiesTab(ChangedHandler changeHandler) {
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			propertiesTabPanel.removeMember(propertiesPanel);
		}
		propertiesPanel = new FolderStandardPropertiesPanel(folder, changeHandler);
		propertiesTabPanel.addMember(propertiesPanel);
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public void setFolder(GUIFolder folder) {
		this.folder = folder;
		refresh();
	}

	public void onModified() {
		enableSave();
	}

	private boolean validate() {
		boolean valid = propertiesPanel.validate();
		if (!valid)
			tabSet.selectTab(0);

		valid = validateExtendedAttributes(valid);

		valid = validateQuota(valid);

		if (valid && interfacePanel != null) {
			valid = interfacePanel.validate();
			if (!valid)
				tabSet.selectTab(interfaceTab);
		}

		if (valid && ocrPanel != null && Feature.enabled(Feature.OCR)) {
			valid = ocrPanel.validate();
			if (!valid)
				tabSet.selectTab(captureTab);
		}

		return valid;
	}

	private boolean validateQuota(boolean valid) {
		if (valid && quotaPanel != null && folder.isWorkspace() && Feature.enabled(Feature.QUOTAS)) {
			valid = quotaPanel.validate();
			if (!valid)
				tabSet.selectTab(quotaTab);
		}
		return valid;
	}

	private boolean validateExtendedAttributes(boolean valid) {
		if (valid && Feature.enabled(Feature.TEMPLATE)) {
			valid = extendedPropertiesPanel.validate();
			if (!valid)
				tabSet.selectTab(1);
		}
		return valid;
	}

	public void onSave() {
		if (validate()) {
			folder.setName(folder.getName().trim());
			FolderService.Instance.get().save(folder, new AsyncCallback<GUIFolder>() {
				@Override
				public void onFailure(Throwable caught) {
					if (caught instanceof ServerValidationException sve) {
						handleValidationException(sve);
					} else {
						GuiLog.serverError(caught);
					}
				}

				@Override
				public void onSuccess(GUIFolder folder) {
					disableSave();
					FolderDetailsPanel.this.folder = folder;

					GUIFolder current = FolderController.get().getCurrentFolder();
					current.setName(folder.getName());
					current.setTemplate(folder.getTemplate());
					current.setTemplateId(folder.getTemplateId());
					current.setAttributes(folder.getAttributes());
					current.setOcrTemplateId(folder.getOcrTemplateId());
					current.setColor(folder.getColor());
					current.setGrid(folder.getGrid());
					current.setTile(folder.getTile());
					current.setId(folder.getId());
					current.setAttributes(folder.getAttributes());
				}
			});
		}
	}

	private void handleValidationException(ServerValidationException e) {
		extendedPropertiesPanel.handleErrors(e);
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	@Override
	protected void onDraw() {
		FolderController.get().addObserver(this);
	}

	private void disableSave() {
		FolderController.get().cancelEditing(folder);
		tabSet.hideSave();
	}

	private void enableSave() {
		FolderController.get().beginEditing(folder);
		tabSet.displaySave();
	}
}