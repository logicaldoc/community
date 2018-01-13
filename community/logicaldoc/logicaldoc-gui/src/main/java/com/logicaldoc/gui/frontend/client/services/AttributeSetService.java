package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;

/**
 * The client side stub for the AttributeSet Service. This service gives all
 * needed methods to handle attribute sets.
 */
@RemoteServiceRelativePath("attributeset")
public interface AttributeSetService extends RemoteService {
	/**
	 * Deletes a given set
	 */
	public void delete(long setId) throws ServerException;

	/**
	 * Creates or updates a set
	 */
	public GUIAttributeSet save(GUIAttributeSet set) throws ServerException;

	/**
	 * Loads a given set from the database
	 */
	public GUIAttributeSet getAttributeSet(long setId) throws ServerException;

	/**
	 * Saves the list of all possible options
	 */
	public void saveOptions(long setId, String attribute, String[] values) throws ServerException;

	/**
	 * Delete a selection of options
	 */
	public void deleteOptions(long setId, String attribute, String[] values) throws ServerException;

	/**
	 * Reads the contacts that are about to be imported from CSV
	 */
	public String[] parseOptions(long setId, String attribute) throws ServerException;

	public static class Instance {
		private static AttributeSetServiceAsync instance;

		public static AttributeSetServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(AttributeSetService.class);
			}
			return instance;
		}
	}
}