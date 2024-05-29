package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIPatch;

public interface UpdateServiceAsync {

	void checkUpdate(AsyncCallback<List<GUIParameter>> callback);

	void downloadUpdate(String id, String fileName, long fileSize, AsyncCallback<Void> callback);

	void checkDownloadStatus(AsyncCallback<List<Integer>> callback);

	void getUpdateNotes(String updateFileName, AsyncCallback<List<String>> callback);

	void confirmUpdate(String updateFileName, AsyncCallback<String> callback);

	void checkPatch(AsyncCallback<List<GUIPatch>> callback);

	void confirmPatch(String updateFileName, AsyncCallback<String> callback);

	void getPatchNotes(String patchFileName, AsyncCallback<List<String>> callback);

	void downloadPatch(String id, String fileName, long fileSize, AsyncCallback<Void> callback);

	void loadUpdate(AsyncCallback<String> callback);

	void deleteUpdate(String pupdateFileName, AsyncCallback<Void> callback);

	void loadPatch(AsyncCallback<String> callback);

	void deletePatch(String patchFileName, AsyncCallback<Void> callback);
}