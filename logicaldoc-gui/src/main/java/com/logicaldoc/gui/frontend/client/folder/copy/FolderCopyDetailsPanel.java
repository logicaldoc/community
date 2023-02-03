package com.logicaldoc.gui.frontend.client.folder.copy;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.folder.FolderInterfacePanel;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects all folders details needed by a folder's copy operation.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class FolderCopyDetailsPanel extends VLayout {

	protected GUIFolder folder = new GUIFolder();

	protected Layout propertiesTabPanel;

	protected Layout extendedPropertiesTabPanel;

	protected Layout interfaceTabPanel;

	protected FolderCopyStandardPropertiesPanel propertiesPanel;

	protected FolderCopyExtendedPropertiesPanel extendedPropertiesPanel;

	protected FolderInterfacePanel interfacePanel;

	protected TabSet tabSet = new TabSet();

	protected Tab propertiesTab;

	protected Tab extendedPropertiesTab;

	protected Tab interfaceTab = null;

	public FolderCopyDetailsPanel(GUIFolder metadata) {
		super();

		this.folder = metadata;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		prepareTabs();
		prepareTabset();

		refresh();
	}

	protected void prepareTabs() {
		propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);

		extendedPropertiesTab = new Tab(I18N.message("propertiesext"));
		extendedPropertiesTabPanel = new HLayout();
		extendedPropertiesTabPanel.setWidth100();
		extendedPropertiesTabPanel.setHeight100();
		extendedPropertiesTab.setPane(extendedPropertiesTabPanel);

		if (Menu.enabled(Menu.FOLDER_INTERFACE)) {
			interfaceTab = new Tab(I18N.message("userinterface"));
			interfaceTabPanel = new HLayout();
			interfaceTabPanel.setWidth100();
			interfaceTabPanel.setHeight100();
			interfaceTab.setPane(interfaceTabPanel);
			tabSet.addTab(interfaceTab);
		}
	}

	protected void prepareTabset() {
		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		tabSet.addTab(propertiesTab);
		tabSet.addTab(extendedPropertiesTab);

		if (Menu.enabled(Menu.FOLDER_INTERFACE))
			tabSet.addTab(interfaceTab);

		addMember(tabSet);
	}

	protected void refresh() {
		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		propertiesPanel = new FolderCopyStandardPropertiesPanel(folder);
		propertiesTabPanel.addMember(propertiesPanel);

		/*
		 * Prepare the extended properties tab
		 */
		if (extendedPropertiesPanel != null) {
			extendedPropertiesPanel.destroy();
			if (Boolean.TRUE.equals(extendedPropertiesTabPanel.contains(extendedPropertiesPanel)))
				extendedPropertiesTabPanel.removeMember(extendedPropertiesPanel);
		}

		extendedPropertiesPanel = new FolderCopyExtendedPropertiesPanel(folder);
		extendedPropertiesTabPanel.addMember(extendedPropertiesPanel);

		if (Menu.enabled(Menu.FOLDER_INTERFACE)) {
			/*
			 * Prepare the User Interface tab
			 */
			if (interfacePanel != null) {
				interfacePanel.destroy();
				interfaceTabPanel.removeMember(interfacePanel);
			}
			interfacePanel = new FolderInterfacePanel(folder, null);
			interfaceTabPanel.addMember(interfacePanel);
		}
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		boolean extValid = extendedPropertiesPanel.validate();
		boolean interfaceValid = true;
		if (interfacePanel != null)
			interfaceValid = interfacePanel.validate();

		if (!stdValid)
			tabSet.selectTab(propertiesTab);
		else if (!extValid)
			tabSet.selectTab(extendedPropertiesTab);
		else if (!interfaceValid)
			tabSet.selectTab(interfaceTab);
		return stdValid && extValid && interfaceValid;
	}
}