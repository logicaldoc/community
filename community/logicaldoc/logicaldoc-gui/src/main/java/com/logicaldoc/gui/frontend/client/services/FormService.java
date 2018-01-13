package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * The client side stub for the Form Service. This service gives all needed
 * methods to handle the forms.
 */
@RemoteServiceRelativePath("form")
public interface FormService extends RemoteService {
	/**
	 * Creates a new form
	 */
	public GUIDocument create(GUIDocument form) throws ServerException;

	/**
	 * Creates a form
	 */
	public void delete(long formId) throws ServerException;

	public static class Instance {
		private static FormServiceAsync instance;

		public static FormServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(FormService.class);
			}
			return instance;
		}
	}
}