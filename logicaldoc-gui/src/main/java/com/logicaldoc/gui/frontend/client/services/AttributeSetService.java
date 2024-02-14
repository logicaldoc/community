package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the AttributeSet Service. This service gives all
 * needed methods to handle attribute sets
 */
@RemoteServiceRelativePath("attributeset")
public interface AttributeSetService extends RemoteService {
	/**
	 * Deletes a given set
	 * 
	 * @param setId identifier of the set
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long setId) throws ServerException;

	/**
	 * Creates or updates a set
	 * 
	 * @param set the set to save
	 * 
	 * @return the saved set
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAttributeSet save(GUIAttributeSet set) throws ServerException;

	/**
	 * Loads a given set from the database
	 * 
	 * @param setId identifier of the set
	 * 
	 * @return the set retrieved from the server
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAttributeSet getAttributeSet(long setId) throws ServerException;

	/**
	 * Loads all the sets from the database
	 * 
	 * @return the sets retrieved from the server
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIAttributeSet> getAttributeSets() throws ServerException;

	/**
	 * Saves the list of all possible options
	 * 
	 * @param setId identifier of the set
	 * @param attribute name of the attribute
	 * @param options possible options
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveOptions(long setId, String attribute, List<GUIValue> options) throws ServerException;

	/**
	 * Delete a selection of options
	 * 
	 * @param setId identifier of the set
	 * @param attribute name of the attribute
	 * @param values collectiojn of options to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteOptions(long setId, String attribute, List<String> values) throws ServerException;

	/**
	 * Reads the contacts that are about to be imported from CSV
	 * 
	 * @param setId identifier of the set
	 * @param attribute name of the attribute
	 * 
	 * @return array of options
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIValue> parseOptions(long setId, String attribute) throws ServerException;

	/**
	 * Forces all the settings of an attribute to all those templates that use
	 * it
	 * 
	 * @param setId identifier of the set
	 * @param attribute name of the attribute
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	void applyAllToTemplates(long setId, String attribute) throws ServerException;

	/**
	 * Forces the validation of an attribute to all those templates that use it
	 * 
	 * @param setId identifier of the set
	 * @param attribute name of the attribute
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	void applyValidationToTemplates(long setId, String attribute) throws ServerException;

	/**
	 * Forces the initialization of an attribute to all those templates that use
	 * it
	 * 
	 * @param setId identifier of the set
	 * @param attribute name of the attribute
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	void applyInitializationToTemplates(long setId, String attribute) throws ServerException;

	public static class Instance {
		private static AttributeSetServiceAsync inst;

		private Instance() {
		}

		public static AttributeSetServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(AttributeSetService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}