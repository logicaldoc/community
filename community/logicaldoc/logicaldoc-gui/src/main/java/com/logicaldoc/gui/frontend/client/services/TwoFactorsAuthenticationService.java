package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * Service to setup the Two Factors Authentication
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
@RemoteServiceRelativePath("twoFactorsAuth")
public interface TwoFactorsAuthenticationService extends RemoteService {

	/**
	 * Creates new credentials of the given account name.
	 * 
	 * @param account the account name
	 * @returns secret key and url of the QRCode
	 */
	public String[] generateGoogleAuthorizationCredentials(String account) throws ServerException;

	/**
	 * Saves the YubiKey credentials in the given user.
	 * 
	 * @param key the key inputed using the USB device
	 * @return The extracted publicId
	 */
	public String generateYubiKeyCredentials(String key) throws ServerException;

	/**
	 * Changed the two factors authentication for a user
	 * 
	 * @param userId The user Identifier
	 * @param secondFactor The authenticator to use
	 * @param key A secret key
	 * @param account An account reference
	 * @param notify If the new settings have to be notified
	 */
	void changeTwoFactorsAuthentication(long userId, String secondFactor, String key, String account, boolean notify)
			throws ServerException;

	public static class Instance {
		private static TwoFactorsAuthenticationServiceAsync instance;

		public static TwoFactorsAuthenticationServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(TwoFactorsAuthenticationService.class);
			}
			return instance;
		}
	}
}