package com.logicaldoc.gui.frontend.client.settings;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.tenant.TenantKeystorePanel;

/**
 * Handles the keystore of the current tenant.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class KeystorePanel extends AdminPanel {
	public KeystorePanel() {
		super("keystore");
		body.setMembers(new TenantKeystorePanel(Session.get().getTenantId()));
	}
}
