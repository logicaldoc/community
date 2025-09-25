package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Bas class for creating second factor authenticators setup panels
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class TwoFactorsAuthenticationSetup extends VLayout {

	protected String key;

	protected String account;
	
	public TwoFactorsAuthenticationSetup() {
		super();
	}

	public String getKey() {
		return key;
	}

	public String getAccount() {
		return account;
	}
	
	public boolean validate() {
		return true;
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}