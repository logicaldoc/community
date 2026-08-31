package com.logicaldoc.gui.setup.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.setup.client.SetupInfo;

public interface SetupServiceAsync {

	void setup(SetupInfo data, AsyncCallback<Void> callback);

	void securityCheck(AsyncCallback<Void> callback);

}
