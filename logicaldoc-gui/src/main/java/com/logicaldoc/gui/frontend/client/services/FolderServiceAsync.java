package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface FolderServiceAsync {

	void save(GUIFolder folder, AsyncCallback<GUIFolder> callback);

	void applyRights(GUIFolder folder, boolean subfolders, AsyncCallback<Void> callback);

	void delete(long[] folderIds, AsyncCallback<Void> callback);

	void getFolder(long folderId, boolean computePath, boolean computeDocs, boolean computeSubfolders,
			AsyncCallback<GUIFolder> callback);

	void move(long[] folderIds, long targetId, AsyncCallback<Void> callback);

	void rename(long folderId, String name, AsyncCallback<Void> callback);

	void paste(long[] docIds, long folderId, String action, AsyncCallback<Void> callback);

	void pasteAsAlias(long[] docIds, long folderId, String type, AsyncCallback<Void> callback);

	void loadTemplates(AsyncCallback<GUIValue[]> callback);

	void saveTemplates(GUIValue[] templates, AsyncCallback<Void> callback);

	void applyTemplate(long folderId, long templateId, boolean inheritSecurity, AsyncCallback<Void> callback);

	void applyMetadata(long parentId, AsyncCallback<Void> callback);

	void create(GUIFolder folder, boolean inheritSecurity, AsyncCallback<GUIFolder> callback);

	void restore(Long[] folderIds, long parentId, AsyncCallback<Void> callback);

	void copyFolders(long[] folderIds, long targetId, boolean foldersOnly, String securityOption, GUIFolder model,
			AsyncCallback<Void> callback);

	void inheritRights(long folderId, long rightsFolderId, AsyncCallback<GUIFolder> callback);

	void deleteFromTrash(Long[] ids, AsyncCallback<Void> callback);

	void createAlias(long parentId, long foldRef, AsyncCallback<GUIFolder> callback);

	void applyTags(long parentId, AsyncCallback<Void> callback);

	void computeStats(long folerId, AsyncCallback<int[]> callback);

	void setFolderPagination(long folderId, Integer startRecord, Integer pageSize, AsyncCallback<Void> callback);

	void applyGridLayout(long folderId, AsyncCallback<Void> callback);

	void applyOCR(long parentId, AsyncCallback<Void> callback);

	void applyStorage(long parentId, AsyncCallback<Void> callback);

	void merge(long[] folderIds, long targetId, AsyncCallback<Void> callback);
}