package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface ChatServiceAsync {

	void post(String message, AsyncCallback<Void> callback);

	void invite(List<String> users, String invitation, AsyncCallback<Void> callback);
}