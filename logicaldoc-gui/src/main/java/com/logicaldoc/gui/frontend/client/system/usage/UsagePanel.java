package com.logicaldoc.gui.frontend.client.system.usage;

import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the system usage grid
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class UsagePanel extends VLayout {

	public UsagePanel() {
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		setMembers(new UsageGrid());
	}
}