package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
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
	 * 
	 * @return secret key and url of the QRCode
	 * 
	 * @throws ServerException error in the server application
	 */
	public String[] generateGoogleAuthorizationCredentials(String account) throws ServerException;

	/**
	 * Saves the YubiKey credentials in the given user.
	 * 
	 * @param key the key inputed using the USB device
	 * 
	 * @return The extracted publicId
	 * 
	 * @throws ServerException error in the server application
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
	 * 
	 * @throws ServerException error in the server application
	 */
	void changeTwoFactorsAuthentication(long userId, String secondFactor, String key, String account, boolean notify)
			throws ServerException;

	public static class Instance {
		private static TwoFactorsAuthenticationServiceAsync instance;

		public static TwoFactorsAuthenticationServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(TwoFactorsAuthenticationService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}