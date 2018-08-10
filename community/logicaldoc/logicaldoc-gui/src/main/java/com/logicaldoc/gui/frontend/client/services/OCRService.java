package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * The client side stub for the OCR Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.4
 */
@RemoteServiceRelativePath("ocr")
public interface OCRService extends RemoteService {
	/**
	 * Loads the OCR settings
	 */
	public GUIParameter[] loadSettings() throws ServerException;

	public static class Instance {
		private static OCRServiceAsync instance;

		public static OCRServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(OCRService.class);
			}
			return instance;
		}
	}
}