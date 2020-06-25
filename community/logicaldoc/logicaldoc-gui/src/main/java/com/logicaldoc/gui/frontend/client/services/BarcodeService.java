package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * The client side stub for the Barcode Engine Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
@RemoteServiceRelativePath("barcode")
public interface BarcodeService extends RemoteService {

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
	public void markUnprocessable(long[] ids) throws ServerException;

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
	public GUIBarcodeTemplate save(GUIBarcodeTemplate template) throws ServerException;

	/**
	 * Loads a given template from the database
	 * 
	 * @param templateId identifier of the template
	 * 
	 * @return the template retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIBarcodeTemplate getTemplate(long templateId) throws ServerException;

	
	/**
	 * Processes the given document
	 * 
	 * @param docId identifier of the document to process
	 * 
	 * @return the processed document's representation
	 * 
	 * @throws ServerException an error happened during the barcode processing
	 */
	public GUIDocument process(long docId) throws ServerException;
	
	public static class Instance {
		private static BarcodeServiceAsync instance;

		public static BarcodeServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(BarcodeService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}