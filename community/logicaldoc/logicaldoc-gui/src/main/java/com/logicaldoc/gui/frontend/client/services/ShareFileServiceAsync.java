package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface ShareFileServiceAsync {

	void exportDocuments(String targetFolder, long[] folderIds, long[] docIds,
			AsyncCallback<Boolean> callback);

	void importDocuments(long targetFolder, String[] itemIds, AsyncCallback<Integer> callback);

	void loadSettings(AsyncCallback<String[]> callback);

	void saveSettings(String hostname, String username, String password, AsyncCallback<Void> callback);

}
