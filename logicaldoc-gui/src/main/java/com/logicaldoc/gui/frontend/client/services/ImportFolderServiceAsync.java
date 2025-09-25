package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;

public interface ImportFolderServiceAsync {
	
	void get(long id, AsyncCallback<GUIImportFolder> callback);
	
	void delete(long id, AsyncCallback<Void> callback);

	void save(GUIImportFolder share, AsyncCallback<GUIImportFolder> callback);

	void test(long id, AsyncCallback<Boolean> callback);

	void changeStatus(long id, boolean enabled, AsyncCallback<Void> callback);

	void resetCache(long id, AsyncCallback<Void> callback);

	void clone(long id, AsyncCallback<GUIImportFolder> callback);

	void resetCounter(long id, AsyncCallback<Void> callback);
}