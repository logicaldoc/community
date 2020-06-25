package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;

/**
 * The client side stub for the Message Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("message")
public interface MessageService extends RemoteService {

	/**
	 * Gets the Message
	 * 
	 * @param messageId identifier of the message
	 * @param markAsRead flag to mark the message as read
	 * 
	 * @return the message instance
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIMessage getMessage(long messageId, boolean markAsRead) throws ServerException;

	/**
	 * Deletes a list of Messages
	 * 
	 * @param ids identifiers of the messages
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long[] ids) throws ServerException;

	void save(GUIMessage message, long[] recipientIds) throws ServerException;

	/**
	 * Loads the templates configured for a given language and type
	 * 
	 * @param language the language of the template
	 * @param type the type of the template
	 * 
	 * @return the list of templates loaded by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIMessageTemplate[] loadTemplates(String language, String type) throws ServerException;

	/**
	 * Gets the template
	 * 
	 * @param templateId identifier of the message template
	 * 
	 * @return the template definition
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIMessageTemplate getTemplate(long templateId) throws ServerException;

	/**
	 * Saves the given templates
	 * 
	 * @param templates the templates to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveTemplates(GUIMessageTemplate[] templates) throws ServerException;

	/**
	 * Deletes a selection of templates
	 * 
	 * @param ids identifiers of the templates to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteTemplates(long[] ids) throws ServerException;

	/**
	 * Deletes the templates with the given name
	 * 
	 * @param name name of the templates to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteTemplates(String name) throws ServerException;
	
	public static class Instance {
		private static MessageServiceAsync instance;

		public static MessageServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(MessageService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
} 