package com.logicaldoc.gui.login.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface TfaServiceAsync {

	void generateKey(String username, AsyncCallback<String> callback);
}