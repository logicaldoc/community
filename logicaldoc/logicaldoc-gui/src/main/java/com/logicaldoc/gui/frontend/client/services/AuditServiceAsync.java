package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface AuditServiceAsync {

	void subscribeFolder(long folderId, boolean currentOnly, String[] events, Long user, Long group,
			AsyncCallback<Void> callback);

	void subscribeDocuments(long[] docIds, String[] events, Long user, Long group,
			AsyncCallback<Void> callback);

	void deleteSubscriptions(long[] ids, AsyncCallback<Void> callback);

	void update(long[] ids, boolean currentOnly, String[] events, AsyncCallback<Void> callback);
}
