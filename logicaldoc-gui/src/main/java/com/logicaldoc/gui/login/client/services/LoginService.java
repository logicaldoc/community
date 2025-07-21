package com.logicaldoc.gui.login.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the Login Service. This service gives all needed
 * methods to handle the login operations.
 */
@RemoteServiceRelativePath("login")
public interface LoginService extends RemoteService {
	/**
	 * Changes the password of a user
	 * 
	 * @param userId The user Identifier
	 * @param oldPassword can be null or is the old password
	 * @param newPassword the new password
	 * 
	 * @return the error code and message. 0 if all went ok, 1 if the password
	 *         is incorrect, 2 if the new password cannot be notified, 3 if the
	 *         password has been already used, otherwise a positive number
	 *         grater than 3
	 */
	public GUIValue changePassword(long userId, String oldPassword, String newPassword);

	/**
	 * Reset the password for the given email.
	 * 
	 * @param username the username for which reset password
	 * @param emailAddress the email for which reset password
	 * @param productName the application product name
	 * 
	 * @throws ServerException error in the server application
	 */
	public void resetPassword(String username, String emailAddress, String productName) throws ServerException;

	/**
	 * Check if a secret key must be provided by the user
	 * 
	 * @param username the username trying to login
	 * @param deviceId identifier of the current device
	 * 
	 * @return True only if the user must provide the secret key
	 * 
	 * @throws ServerException if the user is unexisting or any kind of server
	 *         error
	 */
	public boolean isSecretKeyRequired(String username, String deviceId) throws ServerException;

	public GUIUser getUser(String username);

	/**
	 * Generates a password using the configured policies.
	 * 
	 * @param username the current user
	 * 
	 * @return the generated password
	 */
	public String generatePassword(String username);

	/**
	 * Returns the legals the current user is required to confirm 
	 * 
	 * @param username The user to check
	 * 
	 * @return The legals (name-title)
	 * 
	 * @throws ServerException Error retrieving the legals information
	 */
	public List<GUIParameter> getLegalsToConfirm(String username) throws ServerException;

	/**
	 * Confirms the reading and understanding of a specific legal
	 * 
	 * @param username The user that takes the action
	 * @param legal The name of the legal to confirm
	 * 
	 * @throws ServerException Error marking the legal as read
	 */
	public void confirmLegal(String username, String legal) throws ServerException;

	
	public static class Instance {
		private static LoginServiceAsync inst;

		private Instance() {

		}

		public static LoginServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(LoginService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}
