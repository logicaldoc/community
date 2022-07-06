package com.logicaldoc.gui.frontend.client.settings;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.tenant.TenantKeystorePanel;

/**
 * Handles the settings of the Virtual Agent.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.8
 */
public class KeystorePanel extends AdminPanel {
	public KeystorePanel() {
		super("keystore");
		body.setMembers(new TenantKeystorePanel(Session.get().getTenantId()));
	}
}
