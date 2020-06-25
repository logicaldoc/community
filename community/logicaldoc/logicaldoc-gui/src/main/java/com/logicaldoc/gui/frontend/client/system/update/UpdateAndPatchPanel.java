package com.logicaldoc.gui.frontend.client.system.update;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Updates check panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class UpdateAndPatchPanel extends AdminPanel {

	public UpdateAndPatchPanel() {
		super("updates");
	}

	@Override
	public void onDraw() {
		tab.setPane(new UpdatePanel());

		Tab plugins = new Tab();
		plugins.setTitle(I18N.message("patches"));
		plugins.setPane(new PatchPanel());
		tabs.addTab(plugins);
	}
}