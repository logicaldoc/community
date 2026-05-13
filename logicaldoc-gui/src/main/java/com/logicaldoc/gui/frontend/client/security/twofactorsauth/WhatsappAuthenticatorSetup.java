package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

/**
 * Panel for setting up the Wahtsapp second factor.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class WhatsappAuthenticatorSetup extends TwoFactorsAuthenticationSetup {

	public WhatsappAuthenticatorSetup() {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}
}