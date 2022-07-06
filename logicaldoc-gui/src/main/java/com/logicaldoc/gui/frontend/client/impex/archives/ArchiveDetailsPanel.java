package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects all archives details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ArchiveDetailsPanel extends VLayout {
	private Layout versionsTabPanel;

	private VersionsPanel versionsPanel;

	private TabSet tabSet = new TabSet();

	private Long archiveId;

	private boolean readonly = false;
	
	private ExportArchivesList archivesList = null;

	public ArchiveDetailsPanel(ExportArchivesList list, long archiveId, boolean readonly) {
		super();

		this.archiveId = archiveId;
		this.readonly = readonly;
		this.archivesList = list;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		Tab versionsTab = new Tab(I18N.message("documents"));
		versionsTabPanel = new HLayout();
		versionsTabPanel.setWidth100();
		versionsTabPanel.setHeight100();
		versionsTab.setPane(versionsTabPanel);
		tabSet.addTab(versionsTab);

		addMember(tabSet);

		refresh();
	}

	private void refresh() {
		/*
		 * Prepare the versions tab
		 */
		if (versionsPanel != null) {
			versionsPanel.destroy();
			if (versionsTabPanel.contains(versionsPanel))
				versionsTabPanel.removeMember(versionsPanel);
		}
		versionsPanel = new VersionsPanel(archivesList, archiveId, readonly);
		versionsTabPanel.addMember(versionsPanel);

	}
}