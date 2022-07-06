package com.logicaldoc.gui.login.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * Utility method to handle the two factor authentications from the login page
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.2
 */
@RemoteServiceRelativePath("tfa")
public interface TfaService extends RemoteService {

	/**
	 * Notifies the server to send a secret key for the user
	 * 
	 * @param username the username trying to logn
	 * 
	 * @return a transaction ID
	 * 
	 * @throws ServerException An error happened in the server.
	 */
	public String generateKey(String username) throws ServerException;

	public static class Instance {
		private static TfaServiceAsync instance;

		public static TfaServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(TfaService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}