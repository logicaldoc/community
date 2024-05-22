package com.logicaldoc.core.communication.oauth;

import java.io.IOException;

/**
 * For connecting to the mail store via IMAP or SMTP using OAuth 2.0 authentication,
 * one shall first obtain an access token.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public interface TokenProvider {

	/**
	 * Retrieves the access token for a given email account
	 * 
	 * @return The access token
	 * 
	 * @throws IOException Error retrieving the token 
	 */
	public String getAccessToken() throws IOException;
}