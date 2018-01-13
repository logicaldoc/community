package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates an account
	 */
	public GUIEmailAccount save(GUIEmailAccount account) throws ServerException;

	/**
	 * Loads a given account from the database
	 */
	public GUIEmailAccount get(long id) throws ServerException;

	/**
	 * Test the connection to the given account
	 */
	public boolean test(long id) throws ServerException;

	/**
	 * Changes an account enabled/disabled status
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Cleans the cache
	 */
	public void resetCache(long id) throws ServerException;

	public static class Instance {
		private static EmailAccountServiceAsync instance;

		public static EmailAccountServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(EmailAccountService.class);
			}
			return instance;
		}
	}
}