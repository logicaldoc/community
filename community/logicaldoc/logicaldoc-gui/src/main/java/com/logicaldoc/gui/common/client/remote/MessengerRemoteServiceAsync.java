package com.logicaldoc.gui.common.client.remote;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface MessengerRemoteServiceAsync {
    void start(AsyncCallback<Void> callback);
}