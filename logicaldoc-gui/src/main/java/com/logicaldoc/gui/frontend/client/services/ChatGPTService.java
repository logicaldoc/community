package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the ChatGPT AI.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.40
 */
@RemoteServiceRelativePath("chatgpt")
public interface ChatGPTService extends RemoteService {

	/**
	 * Loads the ChatGPT settings
	 * 
	 * @return The list of settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIValue> loadSettings() throws ServerException;

	/**
	 * Saves the ChatGPT settings
	 * 
	 * @param settings The list of settings to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveSettings(List<GUIValue> settings) throws ServerException;

	/**
	 * Starts a new thread with ChatGPT
	 * 
	 * @param initialQuestion The first question to ask
	 * @param documents The population of documents to use
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void startThread(String initialQuestion, List<GUIDocument> documents) throws ServerException;
	
	/**
	 * Sends a question to ChatGPT in the current thread
	 * 
	 * @param question The question to ask
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void ask(String question) throws ServerException;

	/**
	 * Gets the partial answer to the last question
	 * 
	 * @return key is one of 'complete' or 'partial', value is the consolidated
	 *         response until now
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIValue getAnswer() throws ServerException;

	public static class Instance {
		private static ChatGPTServiceAsync inst;

		private Instance() {
		}

		public static ChatGPTServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ChatGPTService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}