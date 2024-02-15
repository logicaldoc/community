package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface ClusterServiceAsync {

	void makeGlobal(List<String> parameters, AsyncCallback<Void> callback);

	void makeLocal(List<String> parameters, AsyncCallback<Void> callback);
}