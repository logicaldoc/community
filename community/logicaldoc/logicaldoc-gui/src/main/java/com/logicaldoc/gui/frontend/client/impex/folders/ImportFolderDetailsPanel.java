package com.logicaldoc.gui.frontend.client.impex.folders;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ImportFolderService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
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
 * This panel collects details about an import folder
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ImportFolderDetailsPanel extends VLayout {
	private GUIImportFolder importFolder;

	private Layout standardTabPanel;

	private Layout advancedTabPanel;

	private ImportFolderStandardProperties standardPanel;

	private ImportFolderAdvancedProperties advancedPanel;

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private ImportFoldersPanel foldersPanel;

	public ImportFolderDetailsPanel(ImportFoldersPanel foldersPanel) {
		super();

		this.foldersPanel = foldersPanel;
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
				if (importFolder.getId() != 0) {
					ImportFolderService.Instance.get().getImportFolder(importFolder.getId(),
							new AsyncCallback<GUIImportFolder>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
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
				savePanel.setVisible(false);
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

		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

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

		addMember(tabSet);
	}

	private void refresh() {
		if (savePanel != null)
			savePanel.setVisible(false);

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
	}

	public GUIImportFolder getShare() {
		return importFolder;
	}

	public void setShare(GUIImportFolder share) {
		this.importFolder = share;
		refresh();
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		boolean extValid = advancedPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		else if (!extValid)
			tabSet.selectTab(1);
		return stdValid && extValid;
	}

	public void onSave() {
		if (validate()) {
			ImportFolderService.Instance.get().save(importFolder, new AsyncCallback<GUIImportFolder>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIImportFolder share) {
					savePanel.setVisible(false);
					if (share != null) {
						foldersPanel.updateRecord(share);
						foldersPanel.showShareDetails(share);
					}
				}
			});
		}
	}
}