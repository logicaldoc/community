package com.logicaldoc.gui.frontend.client.dropbox;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface DropboxServiceAsync {

	void finishAuthorization(String authorizationCode, AsyncCallback<String> callback);

	void isConnected(AsyncCallback<Boolean> callback);

	void startAuthorization(AsyncCallback<String> callback);

	void exportDocuments(String targetPath, List<Long> folderIds, List<Long> docIds, AsyncCallback<Boolean> callback);

	void importDocuments(long targetFolder, List<String> paths, AsyncCallback<Integer> callback);

	void saveSettings(String clientId, String clientSecret, AsyncCallback<Void> callback);
	
	void loadSettings(AsyncCallback<List<String>> callback);
}