package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing export archives control panel.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ExportArchivesPanel extends AdminPanel {

	public ExportArchivesPanel() {
		super("exportarchives");

		body.setMembers(new ExportArchivesList(GUIArchive.TYPE_DEFAULT, false));

		Tab incremetalTab = new Tab(I18N.message("incrementalarchives"));
		if (Feature.visible(Feature.INCREMENTAL_ARCHIVES)) {
			tabs.addTab(incremetalTab);
			if (!Feature.enabled(Feature.INCREMENTAL_ARCHIVES))
				incremetalTab.setPane(new FeatureDisabled());
			else
				incremetalTab.setPane(new IncrementalArchivesList(GUIArchive.TYPE_DEFAULT));
		}
	}
}