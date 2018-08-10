package com.logicaldoc.gui.frontend.client.system;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * The bottom side of the general panel
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GeneralPanel extends AdminPanel {

	public GeneralPanel() {
		super("general");
	}

	@Override
	public void onDraw() {
		tab.setPane(new StatsPanel());

		Tab sessions = new Tab();
		sessions.setTitle(I18N.message("sessions"));
		sessions.setPane(new SessionsPanel());
		if (Menu.enabled(Menu.ADMIN_SESSIONS))
			tabs.addTab(sessions);

		Tab plugins = new Tab();
		plugins.setTitle(I18N.message("plugins"));
		plugins.setPane(new PluginsPanel());
		tabs.addTab(plugins);

		Tab runlevel = new Tab();
		runlevel.setTitle(I18N.message("runlevel"));
		if (Menu.enabled(Menu.RUNLEVEL) || !Session.get().isDemo()) {
			runlevel.setPane(new RunLevelPanel());
			tabs.addTab(runlevel);
		}

		Tab logs = new Tab();
		logs.setTitle(I18N.message("log"));
		if (Menu.enabled(Menu.LOGS)) {
			logs.setPane(new LogPanel("DMS_WEB"));
			tabs.addTab(logs);
		}
	}
}