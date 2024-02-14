package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUIZone;

/**
 * The client side stub for the Zonal OCR. This service gives all needed methods
 * to handle OCR templates.
 */
@RemoteServiceRelativePath("zonalocr")
public interface ZonalOCRService extends RemoteService {

	/**
	 * Deletes a given template
	 * 
	 * @param templateId identifier of the template
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long templateId) throws ServerException;

	/**
	 * Creates or updates a template
	 * 
	 * @param template the template to save
	 * 
	 * @return the saved template
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIOCRTemplate save(GUIOCRTemplate template) throws ServerException;

	/**
	 * Updates a single zone, if the zone does not exist it will be created
	 * 
	 * @param zone the zone to update
	 * 
	 * @return the newly added zone
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIZone updateZone(GUIZone zone) throws ServerException;

	/**
	 * Loads a given template from the database
	 * 
	 * @param templateId identifier of the template
	 * 
	 * @return the template retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIOCRTemplate getTemplate(long templateId) throws ServerException;

	/**
	 * Processes the given document
	 * 
	 * @param docId identifier of the document to process
	 * 
	 * @return the processed document's representation
	 * 
	 * @throws ServerException an error happened during the Zonal OCR processing
	 */
	public GUIDocument process(long docId) throws ServerException;

	/**
	 * Reschedule all documents for processing
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void rescheduleAll() throws ServerException;

	/**
	 * Marks a set of documents as not processable
	 * 
	 * @param ids document identifiers
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void markUnprocessable(List<Long> ids) throws ServerException;

	public static class Instance {
		private static ZonalOCRServiceAsync inst;

		private Instance() {
		}

		public static ZonalOCRServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ZonalOCRService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}