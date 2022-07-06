package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * Panel for setting up the YubiKey second factor.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class EmailAuthenticatorSetup extends TwoFactorsAuthenticationSetup {

	public EmailAuthenticatorSetup(GUIUser user) {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}
}