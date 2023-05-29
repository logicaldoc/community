package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Chat Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
@RemoteServiceRelativePath("chat")
public interface ChatService extends RemoteService {

	/**
	 * Posts a message to the chat
	 * 
	 * @param message the message text
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void post(String message) throws ServerException;

	/**
	 * Invites the users to join the chat
	 * 
	 * @param users user names to invite
	 * @param invitation the invitation message
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void invite(String[] users, String invitation) throws ServerException;
	
	public static class Instance {
		private static ChatServiceAsync inst;

		private Instance() {}
		
		public static ChatServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ChatService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}