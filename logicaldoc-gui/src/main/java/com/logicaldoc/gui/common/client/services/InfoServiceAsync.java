package com.logicaldoc.gui.common.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

public interface InfoServiceAsync {

	void getInfo(String locale, String tenant, boolean login, AsyncCallback<GUIInfo> callback);

	void getSessionInfo(AsyncCallback<GUIParameter[]> callback);

	void ping(AsyncCallback<Boolean> callback);

	void getCronDescription(String expression, String locale, AsyncCallback<String> callback);
}
