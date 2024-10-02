package com.logicaldoc.gui.frontend.client.onlyoffice;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface OnlyOfficeServiceAsync {

	void startEditing(long docId, AsyncCallback<Void> callback);
	
	void startFilling(long docId, AsyncCallback<Void> callback);

	void endEditing(long docId, AsyncCallback<Void> callback);
}