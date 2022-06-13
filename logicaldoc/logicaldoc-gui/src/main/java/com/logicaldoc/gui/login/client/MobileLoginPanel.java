package com.logicaldoc.gui.login.client;

import com.logicaldoc.gui.common.client.beans.GUIInfo;

/**
 * The panel showing the login form in the mobile web interface
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class MobileLoginPanel extends LoginPanel {

	public MobileLoginPanel(GUIInfo info) {
		super(info);
	}

	@Override
	protected void initGUI() {
		super.initGUI(false);
	}

	@Override
	protected void prepareAlerts() {
		// Do nothing
	}
}