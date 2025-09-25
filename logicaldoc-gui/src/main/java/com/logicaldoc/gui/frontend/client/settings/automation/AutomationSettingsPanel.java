package com.logicaldoc.gui.frontend.client.settings.automation;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Automation settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationSettingsPanel extends AdminPanel {

	public AutomationSettingsPanel() {
		super("automationroutines");

		tab.setPane(new AutomationRoutinesPanel());

		Tab triggersTab = new Tab();
		triggersTab.setTitle(I18N.message("automationtriggers"));
		triggersTab.setPane(new AutomationTriggersPanel());
		tabs.addTab(triggersTab);
	}
}