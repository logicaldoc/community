package com.logicaldoc.gui.frontend.client.onlyoffice;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the OnlyOffice Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
@RemoteServiceRelativePath("onlyoffice")
public interface OnlyOfficeService extends RemoteService {

	/**
	 * Starts the editing of a document with Only Office
	 * 
	 * @param docId ID of the document to edit
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void startEditing(long docId) throws ServerException;

	/**
	 * Starts the filling of a form with Only Office
	 * 
	 * @param docId ID of the form to take as model
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void startFilling(long docId) throws ServerException;

	/**
	 * Starts the editing of a document with Only Office
	 * 
	 * @param docId ID of the document being edited
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void endEditing(long docId) throws ServerException;

	public static class Instance {
		private static OnlyOfficeServiceAsync inst;

		private Instance() {
		}

		public static OnlyOfficeServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(OnlyOfficeService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}