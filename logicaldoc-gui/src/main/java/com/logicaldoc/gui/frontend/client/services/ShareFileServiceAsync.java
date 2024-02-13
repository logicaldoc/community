package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface ShareFileServiceAsync {

	void exportDocuments(String targetFolder, List<Long> folderIds, List<Long> docIds, AsyncCallback<Boolean> callback);

	void importDocuments(long targetFolder, String[] itemIds, AsyncCallback<Integer> callback);

	void loadSettings(AsyncCallback<String[]> callback);

	void authorize(String clientId, String clientSecret, AsyncCallback<String> callback);

	void isAuthorized(AsyncCallback<Boolean> callback);
}