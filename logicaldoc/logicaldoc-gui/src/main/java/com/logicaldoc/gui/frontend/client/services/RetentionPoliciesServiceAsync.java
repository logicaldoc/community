package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIRetentionPolicy;

public interface RetentionPoliciesServiceAsync {

	void delete(long id, AsyncCallback<Void> callback);

	void save(GUIRetentionPolicy policy, AsyncCallback<GUIRetentionPolicy> callback);

	void getPolicy(long id, AsyncCallback<GUIRetentionPolicy> callback);

	void reorder(long[] ids, AsyncCallback<Void> callback);

	void changeStatus(long id, boolean enabled, AsyncCallback<Void> callback);
}
