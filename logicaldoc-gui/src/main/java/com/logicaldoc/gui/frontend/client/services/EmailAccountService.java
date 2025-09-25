package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;

/**
 * The client side stub for the EmailAccount Service. This service gives all
 * needed methods to handle templates.
 */
@RemoteServiceRelativePath("emailaccount")
public interface EmailAccountService extends RemoteService {
	/**
	 * Deletes a given account
	 * 
	 * @param id identifier of the email account
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates an account
	 * 
	 * @param account the account to save
	 * 
	 * @return the saved email account
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIEmailAccount save(GUIEmailAccount account) throws ServerException;

	/**
	 * Loads a given account from the database
	 * 
	 * @param id identifier of the account
	 * 
	 * @return the email account retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIEmailAccount get(long id) throws ServerException;

	/**
	 * Test the connection to the given account
	 * 
	 * @param id identifier of the email account to test
	 * 
	 * @return true if the account has been connected
	 *
	 * @throws ServerException an error happened in the server application
	 */
	public boolean test(long id) throws ServerException;

	/**
	 * Changes an account enabled/disabled status
	 * 
	 * @param id identifier of the account
	 * @param enabled flag indicating is the account must be enabled
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Cleans the cache
	 * 
	 * @param id identifier of the email account
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void resetCache(long id) throws ServerException;
	
	/**
	 * Reset the import counter
	 * 
	 * @param id identifier of the import folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void resetCounter(long id) throws ServerException;

	/**
	 * Clones a given email account
	 * 
	 * @param id Identifier of the original accout to clone
	 * 
	 * @return The created clone
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIEmailAccount clone(long id) throws ServerException;
	
	public static class Instance {
		private static EmailAccountServiceAsync inst;

		private Instance() {
		}
		
		public static EmailAccountServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(EmailAccountService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}