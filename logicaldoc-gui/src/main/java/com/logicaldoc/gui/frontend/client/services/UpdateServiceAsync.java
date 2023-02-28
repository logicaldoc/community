package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIPatch;

public interface UpdateServiceAsync {

	void checkUpdate(AsyncCallback<GUIParameter[]> callback);

	void downloadUpdate(String id, String fileName, long fileSize, AsyncCallback<Void> callback);

	void checkDownloadStatus(AsyncCallback<int[]> callback);

	void getUpdateNotes(String updateFileName, AsyncCallback<String[]> callback);

	void confirmUpdate(String updateFileName, AsyncCallback<String> callback);

	void checkPatch(AsyncCallback<GUIPatch[]> callback);

	void confirmPatch(String updateFileName, AsyncCallback<String> callback);

	void getPatchNotes(String patchFileName, AsyncCallback<String[]> callback);

	void downloadPatch(String id, String fileName, long fileSize, AsyncCallback<Void> callback);
	
	void loadUpdate(AsyncCallback<String> callback);
	
	void loadPatch(AsyncCallback<String> callback);
	
	void getStatus(String fileName, AsyncCallback<String[]> callback);
}