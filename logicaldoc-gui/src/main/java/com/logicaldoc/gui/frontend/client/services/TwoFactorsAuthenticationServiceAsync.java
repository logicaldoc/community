package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface TwoFactorsAuthenticationServiceAsync {

	void generateGoogleAuthorizationCredentials(String account, AsyncCallback<List<String>> callback);

	void changeTwoFactorsAuthentication(long userId, String secondFactor, String key, String account, boolean notify,
			AsyncCallback<Void> callback);

	void generateYubiKeyCredentials(String key, AsyncCallback<String> callback);
}