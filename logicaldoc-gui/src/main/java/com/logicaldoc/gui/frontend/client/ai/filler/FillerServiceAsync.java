package com.logicaldoc.gui.frontend.client.ai.filler;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface FillerServiceAsync {

	void delete(List<Long> fillerIds, AsyncCallback<Void> callback);

	void save(GUIFiller filler, AsyncCallback<GUIFiller> callback);

	void get(long fillerId, AsyncCallback<GUIFiller> callback);
}
