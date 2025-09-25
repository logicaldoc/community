package com.logicaldoc.gui.frontend.client.impex.folders;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.ImportFolderService;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about an import folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportFolderDetailsPanel extends VLayout {
	private GUIImportFolder importFolder;

	private Layout standardTabPanel;

	private Layout advancedTabPanel;

	private Layout automationTabPanel;

	private Layout historyTabPanel;

	private ImportFolderStandardProperties standardPanel;

	private ImportFolderAdvancedProperties advancedPanel;

	private ImportFolderAutomationPanel automationPanel;

	private ImportFolderHistoryPanel historyPanel;

	private EditingTabSet tabSet;

	private ImportFoldersPanel foldersPanel;

	public ImportFolderDetailsPanel(ImportFoldersPanel foldersPanel) {
		super();

		this.foldersPanel = foldersPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (importFolder.getId() != 0) {
				ImportFolderService.Instance.get().get(importFolder.getId(), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIImportFolder share) {
						setShare(share);
					}
				});
			} else {
				GUIImportFolder newshare = new GUIImportFolder();
				newshare.setProvider(importFolder.getProvider());
				setShare(newshare);
			}
			tabSet.hideSave();
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		standardTabPanel = new HLayout();
		standardTabPanel.setWidth100();
		standardTabPanel.setHeight100();
		propertiesTab.setPane(standardTabPanel);
		tabSet.addTab(propertiesTab);

		Tab extendedPropertiesTab = new Tab(I18N.message("propertiesext"));
		advancedTabPanel = new HLayout();
		advancedTabPanel.setWidth100();
		advancedTabPanel.setHeight100();
		extendedPropertiesTab.setPane(advancedTabPanel);
		tabSet.addTab(extendedPropertiesTab);

		Tab automationTab = new Tab(I18N.message("automation"));
		automationTabPanel = new HLayout();
		automationTabPanel.setWidth100();
		automationTabPanel.setHeight100();
		automationTab.setPane(automationTabPanel);
		tabSet.addTab(automationTab);

		Tab historyTab = new Tab(I18N.message("history"));
		historyTabPanel = new HLayout();
		historyTabPanel.setWidth100();
		historyTabPanel.setHeight100();
		historyTab.setPane(historyTabPanel);
		tabSet.addTab(historyTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (standardPanel != null) {
			standardPanel.destroy();
			if (Boolean.TRUE.equals(standardTabPanel.contains(standardPanel)))
				standardTabPanel.removeMember(standardPanel);
		}

		ChangedHandler changeHandler = event -> onModified();

		standardPanel = new ImportFolderStandardProperties(importFolder, changeHandler);
		standardTabPanel.addMember(standardPanel);

		/*
		 * Prepare the extended properties tab
		 */
		if (advancedPanel != null) {
			advancedPanel.destroy();
			if (Boolean.TRUE.equals(advancedTabPanel.contains(advancedPanel)))
				advancedTabPanel.removeMember(advancedPanel);
		}
		advancedPanel = new ImportFolderAdvancedProperties(importFolder, changeHandler);
		advancedTabPanel.addMember(advancedPanel);

		/*
		 * Prepare the automation tab
		 */
		if (automationPanel != null) {
			automationPanel.destroy();
			if (Boolean.TRUE.equals(automationTabPanel.contains(automationPanel)))
				automationTabPanel.removeMember(automationPanel);
		}
		automationPanel = new ImportFolderAutomationPanel(importFolder, changeHandler);
		automationTabPanel.addMember(automationPanel);

		/*
		 * Prepare the history tab
		 */
		if (historyPanel != null) {
			historyPanel.destroy();
			if (Boolean.TRUE.equals(historyTabPanel.contains(historyPanel)))
				historyTabPanel.removeMember(historyPanel);
		}
		historyPanel = new ImportFolderHistoryPanel(importFolder, changeHandler);
		historyTabPanel.addMember(historyPanel);
	}

	public GUIImportFolder getShare() {
		return importFolder;
	}

	public void setShare(GUIImportFolder share) {
		this.importFolder = share;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = true;
		boolean extValid = true;
		boolean histValid = true;

		try {
			stdValid = standardPanel.validate();
		} catch (Exception t) {
			// Nothing to do
		}

		try {
			extValid = advancedPanel.validate();
		} catch (Exception t) {
			// Nothing to do
		}

		boolean automationValid = true;
		try {
			automationValid = automationPanel.validate();
		} catch (Exception t) {
			// Nothing to do
		}

		try {
			histValid = historyPanel.validate();
		} catch (Exception t) {
			// Nothing to do
		}

		if (!stdValid)
			tabSet.selectTab(0);
		else if (!extValid)
			tabSet.selectTab(1);
		else if (!automationValid)
			tabSet.selectTab(3);
		else if (!histValid)
			tabSet.selectTab(3);
		return stdValid && extValid && automationValid && histValid;
	}

	public void onSave() {
		if (validate()) {
			ImportFolderService.Instance.get().save(importFolder, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIImportFolder importFolder) {
					tabSet.hideSave();
					if (importFolder != null) {
						foldersPanel.updateRecord(importFolder);
						foldersPanel.showShareDetails(importFolder);
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