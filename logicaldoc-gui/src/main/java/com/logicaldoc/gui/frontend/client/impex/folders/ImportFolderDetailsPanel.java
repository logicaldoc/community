package com.logicaldoc.gui.frontend.client.impex.folders;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.ImportFolderService;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
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

	private Layout historyTabPanel;

	private ImportFolderStandardProperties standardPanel;

	private ImportFolderAdvancedProperties advancedPanel;

	private ImportFolderHistoryPanel historyPanel;

	private EditingTabSet tabSet;

	private ImportFoldersPanel foldersPanel;

	public ImportFolderDetailsPanel(ImportFoldersPanel foldersPanel) {
		super();

		this.foldersPanel = foldersPanel;
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
				if (importFolder.getId() != 0) {
					ImportFolderService.Instance.get().getImportFolder(importFolder.getId(),
							new AsyncCallback<GUIImportFolder>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

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
			}
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
			if (standardTabPanel.contains(standardPanel))
				standardTabPanel.removeMember(standardPanel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};
		standardPanel = new ImportFolderStandardProperties(importFolder, changeHandler);
		standardTabPanel.addMember(standardPanel);

		/*
		 * Prepare the extended properties tab
		 */
		if (advancedPanel != null) {
			advancedPanel.destroy();
			if (advancedTabPanel.contains(advancedPanel))
				advancedTabPanel.removeMember(advancedPanel);
		}
		advancedPanel = new ImportFolderAdvancedProperties(importFolder, changeHandler);
		advancedTabPanel.addMember(advancedPanel);

		/*
		 * Prepare the history tab
		 */
		if (historyPanel != null) {
			historyPanel.destroy();
			if (historyTabPanel.contains(historyPanel))
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
		} catch (Throwable t) {

		}

		try {
			extValid = advancedPanel.validate();
		} catch (Throwable t) {

		}

		try {
			histValid = historyPanel.validate();
		} catch (Throwable t) {

		}

		if (!stdValid)
			tabSet.selectTab(0);
		else if (!extValid)
			tabSet.selectTab(1);
		else if (!histValid)
			tabSet.selectTab(2);
		return stdValid && extValid && histValid;
	}

	public void onSave() {
		if (validate()) {
			ImportFolderService.Instance.get().save(importFolder, new AsyncCallback<GUIImportFolder>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIImportFolder share) {
					tabSet.hideSave();
					if (share != null) {
						foldersPanel.updateRecord(share);
						foldersPanel.showShareDetails(share);
					}
				}
			});
		}
	}
}