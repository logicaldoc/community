package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

public interface UpdateServiceAsync {

	void checkUpdate(String userNo, String currentRelease, AsyncCallback<GUIParameter[]> callback);

	void download(String userNo, String id, String fileName, int size, AsyncCallback<Void> callback);

	void checkDownloadStatus(AsyncCallback<int[]> callback);

	void getNotes(String updateFileName, AsyncCallback<String[]> callback);

	void confirm(String updateFileName, AsyncCallback<String> callback);
}
