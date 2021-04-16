package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.logicaldoc.gui.frontend.client.workflow.WorkflowTriggersPanel;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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

	private Layout ocrTabPanel;

	private StandardPropertiesPanel propertiesPanel;

	private FolderExtendedPropertiesPanel extendedPropertiesPanel;

	private FolderSecurityPanel securityPanel;

	private HistoryPanel historyPanel;

	private AliasesPanel aliasesPanel;

	private WorkflowTriggersPanel workflowsPanel;

	private FolderSubscriptionsPanel subscriptionsPanel;

	private FolderQuotaPanel quotaPanel;

	private FolderAutomationPanel automationPanel;

	private FolderInterfacePanel interfacePanel;

	private FolderOCRPanel ocrPanel;

	private EditingTabSet tabSet;

	private Tab workflowTab = null;

	private Tab subscriptionsTab = null;

	private Tab quotaTab = null;

	private Tab automationTab = null;

	private Tab interfaceTab = null;

	private Tab ocrTab = null;

	public FolderDetailsPanel(GUIFolder folder) {
		super();

		this.folder = folder;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		}, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				FolderService.Instance.get().getFolder(getFolder().getId(), false, false, false,
						new AsyncCallback<GUIFolder>() {

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
						});
			}
		});

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

		workflowTab = new Tab(I18N.message("workflow"));
		if (folder.hasPermission(Constants.PERMISSION_WORKFLOW))
			if (Feature.visible(Feature.WORKFLOW)) {
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

		subscriptionsTab = new Tab(I18N.message("subscriptions"));
		if (folder.hasPermission(Constants.PERMISSION_SUBSCRIPTION))
			if (Feature.visible(Feature.AUDIT)) {
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

		try {
			quotaTab = new Tab(I18N.message("quota"));
			if (folder.isWorkspace() && folder.hasPermission(Constants.PERMISSION_WRITE))
				if (Feature.visible(Feature.QUOTAS)) {
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
		} catch (Throwable t) {
		}

		Tab aliasesTab = new Tab(I18N.message("aliases"));
		aliasesTabPanel = new HLayout();
		aliasesTabPanel.setWidth100();
		aliasesTabPanel.setHeight100();
		aliasesTab.setPane(aliasesTabPanel);
		if (Menu.enabled(Menu.ALIASES) && folder.getFoldRef() == null)
			tabSet.addTab(aliasesTab);

		automationTab = new Tab(I18N.message("automation"));
		if (folder.hasPermission(Constants.PERMISSION_AUTOMATION))
			if (Feature.visible(Feature.AUTOMATION)) {
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
			ocrTab = new Tab(I18N.message("ocr"));
			ocrTabPanel = new HLayout();
			ocrTabPanel.setWidth100();
			ocrTabPanel.setHeight100();
			ocrTab.setPane(ocrTabPanel);
			tabSet.addTab(ocrTab);
		}

		addMember(tabSet);
		refresh();
	}

	private void refresh() {
		try {
			disableSave();

			ChangedHandler changeHandler = new ChangedHandler() {
				@Override
				public void onChanged(ChangedEvent event) {
					onModified();
				}
			};

			/*
			 * Prepare the standard properties tab
			 */
			if (propertiesPanel != null) {
				propertiesPanel.destroy();
				propertiesTabPanel.removeMember(propertiesPanel);
			}
			propertiesPanel = new StandardPropertiesPanel(folder, changeHandler);
			propertiesTabPanel.addMember(propertiesPanel);

			/*
			 * Prepare the extended properties tab
			 */
			if (extendedPropertiesPanel != null) {
				extendedPropertiesPanel.destroy();
				extendedPropertiesTabPanel.removeMember(extendedPropertiesPanel);
			}

			ChangedHandler templateChangedHandler = new ChangedHandler() {
				@Override
				public void onChanged(ChangedEvent event) {
					folder.setOcrTemplateId(null);
					folder.setBarcodeTemplateId(null);
					ocrPanel.refresh(folder.getTemplateId());
				}
			};
			extendedPropertiesPanel = new FolderExtendedPropertiesPanel(folder, changeHandler, templateChangedHandler);
			if (Feature.enabled(Feature.TEMPLATE))
				extendedPropertiesTabPanel.addMember(extendedPropertiesPanel);

			/*
			 * Prepare the security properties tab
			 */
			if (securityPanel != null) {
				securityPanel.destroy();
				securityTabPanel.removeMember(securityPanel);
			}
			securityPanel = new FolderSecurityPanel(folder);
			securityTabPanel.addMember(securityPanel);

			/*
			 * Prepare the aliases tab
			 */
			if (aliasesPanel != null) {
				aliasesPanel.destroy();
				aliasesTabPanel.removeMember(aliasesPanel);
			}
			aliasesPanel = new AliasesPanel(folder);
			aliasesTabPanel.addMember(aliasesPanel);

			/*
			 * Prepare the history tab
			 */
			if (historyPanel != null) {
				historyPanel.destroy();
				historyTabPanel.removeMember(historyPanel);
			}
			historyPanel = new HistoryPanel(folder);
			historyTabPanel.addMember(historyPanel);

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

			if (Menu.enabled(Menu.CAPTURE)) {
				/*
				 * Prepare the OCR tab
				 */
				if (ocrPanel != null) {
					ocrPanel.destroy();
					ocrTabPanel.removeMember(ocrPanel);
				}
				ocrPanel = new FolderOCRPanel(folder, changeHandler);
				ocrTabPanel.addMember(ocrPanel);
			}
		} catch (Throwable r) {
			GuiLog.error(r.getMessage(), null, r);
		}
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

		if (valid && Feature.enabled(Feature.TEMPLATE)) {
			valid = extendedPropertiesPanel.validate();
			if (!valid)
				tabSet.selectTab(1);
		}

		if (valid && quotaPanel != null && folder.isWorkspace() && Feature.enabled(Feature.QUOTAS)) {
			valid = quotaPanel.validate();
			if (!valid)
				tabSet.selectTab(quotaTab);
		}

		if (valid && interfacePanel != null) {
			valid = interfacePanel.validate();
			if (!valid)
				tabSet.selectTab(interfaceTab);
		}

		if (valid && ocrPanel != null && Feature.enabled(Feature.OCR)) {
			valid = ocrPanel.validate();
			if (!valid)
				tabSet.selectTab(ocrTab);
		}

		return valid;
	}

	public void onSave() {
		if (validate()) {
			folder.setName(folder.getName().trim());
			FolderService.Instance.get().save(folder, new AsyncCallback<GUIFolder>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIFolder folder) {
					disableSave();
					FolderController.get().modified(folder);
					GUIFolder current = Session.get().getCurrentFolder();
					current.setTemplate(folder.getTemplate());
					current.setTemplateId(folder.getTemplateId());
					current.setAttributes(folder.getAttributes());
					current.setOcrTemplateId(folder.getOcrTemplateId());
				}
			});
		}
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {

	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		if (this.folder != null && this.folder.getId() == folder.getId()) {
			setFolder(folder);
		}
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
	public void destroy() {
		FolderController.get().removeObserver(this);
	}

	@Override
	protected void finalize() throws Throwable {
		destroy();
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
		tabSet.hideSave();
	}

	private void enableSave() {
		tabSet.displaySave();
	}
}