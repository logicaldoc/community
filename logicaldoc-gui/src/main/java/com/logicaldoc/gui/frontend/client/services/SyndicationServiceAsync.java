package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISyndication;

public interface SyndicationServiceAsync {

	void changeStatus(long id, boolean enabled, AsyncCallback<Void> callback);

	void delete(long id, AsyncCallback<Void> callback);

	void getSyndication(long id, AsyncCallback<GUISyndication> callback);

	void save(GUISyndication syndication, AsyncCallback<GUISyndication> callback);

	void test(long id, AsyncCallback<Boolean> callback);

	void resetCache(long id, AsyncCallback<Void> callback);

}
