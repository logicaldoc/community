package com.logicaldoc.gui.common.client.websockets;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface WebsocketsMessageServiceAsync {

	void getMessage(EventMessage message, AsyncCallback<EventMessage> callback);

}
