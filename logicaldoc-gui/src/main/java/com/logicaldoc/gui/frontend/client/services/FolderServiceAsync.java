package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface FolderServiceAsync {

	void save(GUIFolder folder, AsyncCallback<GUIFolder> callback);

	void delete(List<Long> folderIds, AsyncCallback<Void> callback);

	void getFolder(long folderId, boolean computePath, boolean computeDocs, boolean computeSubfolders,
			AsyncCallback<GUIFolder> callback);

	void move(List<Long> folderIds, long targetId, AsyncCallback<Void> callback);

	void rename(long folderId, String name, AsyncCallback<Void> callback);

	void paste(List<Long> docIds, long folderId, String action, boolean links, boolean notes, boolean security,
			AsyncCallback<Void> callback);

	void pasteAsAlias(List<Long> docIds, long folderId, String type, AsyncCallback<Void> callback);

	void loadTemplates(AsyncCallback<List<GUIValue>> callback);

	void saveTemplates(List<GUIValue> templates, AsyncCallback<Void> callback);

	void applyTemplate(long folderId, long templateId, boolean inheritSecurity, AsyncCallback<Void> callback);

	void applyMetadata(long parentId, AsyncCallback<Void> callback);

	void create(GUIFolder folder, boolean inheritSecurity, AsyncCallback<GUIFolder> callback);

	void restore(List<Long> folderIds, long parentId, AsyncCallback<Void> callback);

	void copyFolders(List<Long> folderIds, long targetId, boolean foldersOnly, String securityOption, GUIFolder model,
			AsyncCallback<Void> callback);

	void saveACL(GUIFolder folder, boolean subfolders, AsyncCallback<Void> callback);

	void inheritACL(long folderId, long rightsFolderId, AsyncCallback<GUIFolder> callback);

	void deleteFromTrash(List<Long> ids, AsyncCallback<Void> callback);

	void createAlias(long parentId, long foldRef, AsyncCallback<GUIFolder> callback);

	void applyTags(long parentId, AsyncCallback<Void> callback);

	void computeStats(long folerId, AsyncCallback<List<Long>> callback);

	void setFolderPagination(long folderId, Integer startRecord, Integer pageSize, AsyncCallback<Void> callback);

	void applyGridLayout(long folderId, AsyncCallback<Void> callback);

	void applyOCR(long parentId, AsyncCallback<Void> callback);

	void applyStore(long parentId, AsyncCallback<Void> callback);

	void merge(List<Long> folderIds, long targetId, AsyncCallback<Void> callback);

	void readImage(AsyncCallback<String> callback);
}