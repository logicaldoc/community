package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.beans.GUIIncrementalArchive;

public interface ImpexServiceAsync {

	void delete(long archiveId, AsyncCallback<Void> callback);

	void deleteVersions(long archiveId, List<Long> versionIds, AsyncCallback<GUIArchive> callback);

	void save(GUIArchive archive, AsyncCallback<GUIArchive> callback);

	void setStatus(long archiveId, int status, AsyncCallback<Void> callback);

	void deleteIncremental(long id, AsyncCallback<Void> callback);

	void loadIncremental(long id, AsyncCallback<GUIIncrementalArchive> callback);

	void saveIncremental(GUIIncrementalArchive incremental, AsyncCallback<GUIIncrementalArchive> callback);

	void addDocuments(long archiveId, List<Long> documentIds, AsyncCallback<Void> callback);

	void addFolder(long archiveId, long rootId, AsyncCallback<Void> callback);

	void deleteFolder(String folderName, AsyncCallback<Void> callback);

	void startImport(String folderName, AsyncCallback<Void> callback);

	void load(long archiveId, AsyncCallback<GUIArchive> callback);
}
