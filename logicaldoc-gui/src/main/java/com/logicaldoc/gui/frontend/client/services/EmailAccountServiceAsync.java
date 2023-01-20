package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;

public interface EmailAccountServiceAsync {

	void changeStatus(long id, boolean enabled, AsyncCallback<Void> callback);

	void delete(long id, AsyncCallback<Void> callback);

	void get(long id, AsyncCallback<GUIEmailAccount> callback);

	void resetCache(long id, AsyncCallback<Void> callback);

	void save(GUIEmailAccount account, AsyncCallback<GUIEmailAccount> callback);

	void test(long id, AsyncCallback<Boolean> callback);

	void resetCounter(long id, AsyncCallback<Void> callback);
}
