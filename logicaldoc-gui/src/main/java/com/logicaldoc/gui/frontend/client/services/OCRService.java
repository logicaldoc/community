package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * The client side stub for the OCR Service
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.4
 */
@RemoteServiceRelativePath("ocr")
public interface OCRService extends RemoteService {
	/**
	 * Loads the OCR settings
	 * 
	 * @return settings of the OCR engine
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIParameter[] loadSettings() throws ServerException;

	public static class Instance {
		private static OCRServiceAsync inst;

		private Instance() {
		}
		
		public static OCRServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(OCRService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}