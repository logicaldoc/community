package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface ClusterServiceAsync {

	void makeGlobal(String[] parameters, AsyncCallback<Void> callback);

	void makeLocal(String[] parameters, AsyncCallback<Void> callback);

}
