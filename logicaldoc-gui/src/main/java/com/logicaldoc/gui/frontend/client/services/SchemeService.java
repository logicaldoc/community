package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIScheme;

/**
 * The client side stub for the SchemeService Service. This service gives all
 * needed methods to handle custom id /auto naming / auto folding
 * configurations.
 */
@RemoteServiceRelativePath("scheme")
public interface SchemeService extends RemoteService {
	/**
	 * Deletes a template
	 * 
	 * @param templateId identifier of the template
	 * @param type type of the template
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long templateId, String type) throws ServerException;

	/**
	 * Creates or updates a configuration
	 * 
	 * @param scheme the scheme definition
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void save(GUIScheme scheme) throws ServerException;

	/**
	 * Loads a given configuration from the database
	 * 
	 * @param templateId identifier of the template
	 * @param type type of the template
	 * 
	 * @return the scheme retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIScheme get(long templateId, String type) throws ServerException;

	/**
	 * Load all Custom Identifiers rules
	 * 
	 * @return the scheme definitions
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIScheme> load() throws ServerException;

	/**
	 * Reset the numbering of a given sequence
	 * 
	 * @param sequenceId identifier of the sequence
	 * @param value the new sequence's value
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void resetSequence(long sequenceId, long value) throws ServerException;

	/**
	 * Deletes the given sequence
	 * 
	 * @param sequenceId identifiers
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteSequence(long sequenceId) throws ServerException;

	public static class Instance {
		private static SchemeServiceAsync inst;

		private Instance() {
		}

		public static SchemeServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SchemeService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}