package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
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
	 * 
	 * @param ids identifiers of the contacts to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long[] ids) throws ServerException;

	/**
	 * Saves a contact in the database
	 * 
	 * @param contact the contact to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void save(GUIContact contact) throws ServerException;

	/**
	 * Loads a contact from the database
	 * 
	 * @param id identifier of the contact
	 * 
	 * @return the contact retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIContact load(long id) throws ServerException;

	/**
	 * Reads the contacts that are about to be imported from CSV file
	 * 
	 * @param preview if the preview must be shown
	 * @param separator the separator char
	 * @param delimiter the fields delimiter
	 * @param skipFirstRow if the first row contains titles and mus be skipped
	 * @param firstName the first name
	 * @param lastName the last name
	 * @param email the email
	 * @param company the company
	 * @param phone the phone
	 * @param mobile the mobile phone
	 * @param address the address
	 * 
	 * @return the extracted contacts
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIContact[] parseContacts(boolean preview, String separator, String delimiter, boolean skipFirstRow,
			int firstName, int lastName, int email, int company, int phone, int mobile, int address)
			throws ServerException;

	/**
	 * Shares contacts among a set of users and groups
	 * 
	 * @param contactIds identifiers of the contacts to share
	 * @param userIds direct ids of users to share the contacts with
	 * @param groupIds the groups of users to share the contacts with
	 * 
	 * @throws ServerException share the search to
	 */
	void shareContacts(long[] contactIds, long[] userIds, long[] groupIds) throws ServerException;

	public static class Instance {
		private static ContactServiceAsync inst;

		private Instance() {
		}

		public static ContactServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ContactService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}