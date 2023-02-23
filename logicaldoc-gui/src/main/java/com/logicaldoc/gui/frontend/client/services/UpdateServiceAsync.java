package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIPatch;

public interface UpdateServiceAsync {

	void checkUpdate(String userNo, String currentRelease, AsyncCallback<GUIParameter[]> callback);

	void downloadUpdate(String userNo, String id, String fileName, long fileSize, AsyncCallback<Void> callback);

	void checkDownloadStatus(AsyncCallback<int[]> callback);

	void getUpdateNotes(String updateFileName, AsyncCallback<String[]> callback);

	void confirmUpdate(String updateFileName, AsyncCallback<String> callback);

	void checkPatch(String userNo, String currentRelease, AsyncCallback<GUIPatch[]> callback);

	void confirmPatch(String updateFileName, AsyncCallback<String> callback);

	void getPatchNotes(String patchFileName, AsyncCallback<String[]> callback);

	void downloadPatch(String userNo, String id, String fileName, long fileSize, AsyncCallback<Void> callback);
	
	void loadUpdate(AsyncCallback<String> callback);
}
