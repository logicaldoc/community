package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUITemplate;

/**
 * The client side stub for the Template Service. This service gives all needed
 * methods to handle templates.
 */
@RemoteServiceRelativePath("template")
public interface TemplateService extends RemoteService {
	/**
	 * Deletes a given template
	 */
	public void delete(long templateId) throws ServerException;

	/**
	 * Creates or updates a template
	 */
	public GUITemplate save(GUITemplate template) throws ServerException;

	/**
	 * Loads a given template from the database
	 */
	public GUITemplate getTemplate(long templateId) throws ServerException;

	/**
	 * Loads a given template from the database
	 */
	public long countDocuments(long templateId) throws ServerException;

	public static class Instance {
		private static TemplateServiceAsync instance;

		public static TemplateServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(TemplateService.class);
			}
			return instance;
		}
	}
}