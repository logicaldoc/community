package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface DropboxServiceAsync {

	void finishAuthorization(String authorizationCode, AsyncCallback<String> callback);

	void isConnected(AsyncCallback<Boolean> callback);

	void startAuthorization(AsyncCallback<String> callback);

	void exportDocuments(String targetPath, long[] folderIds, Long[] docIds, AsyncCallback<Boolean> callback);

	void importDocuments(long targetFolder, String[] paths, AsyncCallback<Integer> callback);

}
