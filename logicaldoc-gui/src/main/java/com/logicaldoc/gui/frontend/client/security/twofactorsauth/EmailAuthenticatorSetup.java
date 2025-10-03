package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

/**
 * Panel for setting up the Email second factor.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class EmailAuthenticatorSetup extends TwoFactorsAuthenticationSetup {

	public EmailAuthenticatorSetup() {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}
}