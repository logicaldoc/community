package com.logicaldoc.gui.common.client.websockets;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;

/**
 * Dummy service interface only used to simplify serialization/deserialization
 * of MessageEvent object using GWT serialization framework.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
@RemoteServiceRelativePath("WebsocketsMessageService")
public interface WebsocketsMessageService extends RemoteService {

	/**
	 * The RPC Serialization is designed for function call. The client serialize
	 * the function argument and the server the return value. So, if we want
	 * exchange the same object, it’s important that in the service definition
	 * the args and the return value are the same class
	 * 
	 * @param message the event message
	 * 
	 * @return the event message
	 */
	public EventMessage getMessage(EventMessage message);
}