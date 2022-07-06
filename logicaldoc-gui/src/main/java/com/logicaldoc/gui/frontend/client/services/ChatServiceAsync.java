package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface ChatServiceAsync {

	void post(String message, AsyncCallback<Void> callback);

	void invite(String[] users, String invitation, AsyncCallback<Void> callback);

}
