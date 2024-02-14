package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface AuditServiceAsync {

	void subscribeFolder(long folderId, boolean currentOnly, List<String> events, Long user, Long group,
			AsyncCallback<Void> callback);

	void subscribeDocuments(List<Long> docIds, List<String> events, Long user, Long group, AsyncCallback<Void> callback);

	void deleteSubscriptions(List<Long> ids, AsyncCallback<Void> callback);

	void update(List<Long> ids, boolean currentOnly, List<String> events, AsyncCallback<Void> callback);
}
