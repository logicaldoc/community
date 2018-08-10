package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;

/**
 * The client side stub for the Message Service. This service allows r/w
 * operations on folders.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("message")
public interface MessageService extends RemoteService {

	/**
	 * Gets the Message
	 */
	public GUIMessage getMessage(long messageId, boolean markAsRead) throws ServerException;

	/**
	 * Deletes a list of Messages
	 */
	public void delete(long[] ids) throws ServerException;

	void save(GUIMessage message, long[] recipientIds) throws ServerException;

	/**
	 * Loads the templates configured for a given language and type
	 */
	public GUIMessageTemplate[] loadTemplates(String language, String type) throws ServerException;

	/**
	 * Gets the template
	 */
	public GUIMessageTemplate getTemplate(long templateId) throws ServerException;

	/**
	 * Saves the given templates
	 */
	public void saveTemplates(GUIMessageTemplate[] templates) throws ServerException;

	/**
	 * Deletes a selection of templates
	 */
	public void deleteTemplates(long[] ids) throws ServerException;

	/**
	 * Deletes the templates with the given name
	 */
	public void deleteTemplates(String name) throws ServerException;
	
	public static class Instance {
		private static MessageServiceAsync instance;

		public static MessageServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(MessageService.class);
			}
			return instance;
		}
	}
}