package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface GDriveServiceAsync {

	void checkin(long docId, String comment, boolean major, AsyncCallback<GUIDocument> callback);

	void upload(long docId, AsyncCallback<String> callback);

	void delete(String resourceId, AsyncCallback<Void> callback);

	void saveSettings(String clientId, String clientSecret, AsyncCallback<String> callback);

	void importDocuments(List<String> resourceIds, long targetFolderId, String type, AsyncCallback<Void> callback);

	void exportDocuments(List<Long> ids, AsyncCallback<List<String>> callback);

	void search(String expression, AsyncCallback<List<GUIDocument>> callback);

	void create(String fileName, AsyncCallback<String> callback);

	void loadSettings(AsyncCallback<List<String>> callback);

}
