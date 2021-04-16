package com.logicaldoc.gui.login.client.services;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
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
	 * @return the error code and message. 0 if all went ok, 1 if the password is incorrect, 2 if the new
	 *         password cannot be notified, 3 if the password has been already used, otherwise a positive number grater
	 *         than 3
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

	public GUIUser getUser(String username);
}
