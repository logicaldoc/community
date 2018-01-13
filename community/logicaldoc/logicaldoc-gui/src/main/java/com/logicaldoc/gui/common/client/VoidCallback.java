package com.logicaldoc.gui.common.client;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.log.Log;

public final class VoidCallback implements AsyncCallback<Void> {
	@Override
	public void onFailure(Throwable caught) {
		Log.warn(caught.getMessage(), null);
	}

	@Override
	public void onSuccess(Void arg0) {

	}
}