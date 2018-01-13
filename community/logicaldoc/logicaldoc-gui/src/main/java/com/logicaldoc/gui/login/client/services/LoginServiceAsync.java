package com.logicaldoc.gui.login.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;

public interface LoginServiceAsync {

	void changePassword(long userId, String oldPassword, String newPassword, boolean notify,
			AsyncCallback<Integer> callback);

	void getUser(String username, AsyncCallback<GUIUser> callback);

	void resetPassword(String username, String emailAddress, String productName, AsyncCallback<Void> callback);
}
