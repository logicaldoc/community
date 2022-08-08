package com.logicaldoc.gui.login.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface LoginServiceAsync {

	void changePassword(long userId, String oldPassword, String newPassword, AsyncCallback<GUIValue> callback);

	void getUser(String username, AsyncCallback<GUIUser> callback);

	void resetPassword(String username, String emailAddress, String productName, AsyncCallback<Void> callback);

	void isSecretKeyRequired(String username, String deviceId, AsyncCallback<Boolean> callback);

	void generatePassword(String username, AsyncCallback<String> callback);
}
