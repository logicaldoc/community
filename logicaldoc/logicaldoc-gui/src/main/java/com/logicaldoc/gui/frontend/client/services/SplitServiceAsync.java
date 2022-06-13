package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface SplitServiceAsync {

	void split(long docId, int policy, int separator, String expression, AsyncCallback<Void> callback);
}