package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIContact;

/**
 * The client side stub for the Contact Service. This service allows r/w
 * operations on contacts.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("contact")
public interface ContactService extends RemoteService {
	/**
	 * Deletes a selection of contacts
	 */
	public void delete(long[] ids) throws ServerException;

	/**
	 * Saves a contact in the database
	 */
	public void save(GUIContact contact) throws ServerException;

	/**
	 * Loads a contact from the database
	 */
	public GUIContact load(long id) throws ServerException;

	/**
	 * Reads the contacts that are about to beimported from CSV file
	 */
	public GUIContact[] parseContacts(boolean preview, String separator, String delimiter,
			boolean skipFirstRow, int firstName, int lastName, int email, int company, int phone, int mobile,
			int address) throws ServerException;
	
	public static class Instance {
		private static ContactServiceAsync instance;

		public static ContactServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ContactService.class);
			}
			return instance;
		}
	}
}