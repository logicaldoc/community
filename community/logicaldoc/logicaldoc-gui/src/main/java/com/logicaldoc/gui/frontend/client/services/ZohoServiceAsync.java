package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface ZohoServiceAsync {

	void exportDocuments(String targetFolderId, long[] folderIds, long[] docIds, AsyncCallback<Boolean> callback);

	void importDocuments(long targetFolder, String[] folderCompositeIds, String[] documentIds,
			AsyncCallback<Integer> callback);

	void loadAuthToken(AsyncCallback<String> callback);

	void saveAuthToken(String authToken, AsyncCallback<Void> callback);

	void upload(long docId, AsyncCallback<String> callback);

	void delete(String resourceId, AsyncCallback<Void> callback);

	void checkin(long docId, String comment, boolean major, AsyncCallback<GUIDocument> callback);
}
