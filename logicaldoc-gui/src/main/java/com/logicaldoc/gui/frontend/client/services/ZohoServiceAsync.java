package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface ZohoServiceAsync {

	void exportDocuments(String targetFolderId, List<Long> folderIds, List<Long> docIds,
			AsyncCallback<Boolean> callback);

	void importDocuments(long targetFolder, String[] folderCompositeIds, String[] documentIds,
			AsyncCallback<Integer> callback);

	void upload(long docId, AsyncCallback<String> callback);

	void delete(String resourceId, AsyncCallback<Void> callback);

	void checkin(long docId, String comment, boolean major, AsyncCallback<GUIDocument> callback);

	void saveSettings(String clientId, String clientSecret, AsyncCallback<String> callback);

	void loadSettings(AsyncCallback<String[]> callback);
}
