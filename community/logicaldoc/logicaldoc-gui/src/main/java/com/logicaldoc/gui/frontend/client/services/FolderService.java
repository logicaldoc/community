package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the Folder Service. This service allows r/w
 * operations on folders.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
@RemoteServiceRelativePath("folder")
public interface FolderService extends RemoteService {
	/**
	 * Saves the folder in the DB
	 * 
	 * @param folder The folder to save
	 * @return The saved folder
	 */
	public GUIFolder save(GUIFolder folder) throws ServerException;

	/**
	 * Creates a new folder
	 * 
	 * @param newFolder The folder to be created
	 * @param inheritSecurity True if the new folder has to inherit the security
	 *        policies from the parent
	 * @return The saved folder
	 */
	public GUIFolder create(GUIFolder newFolder, boolean inheritSecurity) throws ServerException;

	/**
	 * Creates a new folder alias
	 * 
	 * @param parentId The folder in which the alias must be created
	 * @param foldRef The original folder
	 * @return The created alias
	 */
	public GUIFolder createAlias(long parentId, long foldRef) throws ServerException;

	/**
	 * Renames the given folder
	 */
	public void rename(long folderId, String name) throws ServerException;

	/**
	 * Applies all security settings to folder
	 * 
	 * @param folder The folder that contains the new security settings
	 * @param subfolders If true, the current security settings will be applied
	 *        to the sub-folders
	 */
	public void applyRights(GUIFolder folder, boolean subfolders) throws ServerException;

	/**
	 * Inherits the rights of another folder
	 * 
	 * @param folderId The folder that has to be updated
	 * @param rightsFolderId the folder that defines the rights
	 * 
	 * @return The updated Folder
	 */
	public GUIFolder inheritRights(long folderId, long rightsFolderId) throws ServerException;

	/**
	 * Applies all extended attributes to a sub-tree
	 * 
	 * @param parentId The parent folder containing the metadata
	 */
	public void applyMetadata(long parentId) throws ServerException;

	/**
	 * Applies all tags to a sub-tree
	 * 
	 * @param parentId The parent folder containing the tags
	 */
	public void applyTags(long parentId) throws ServerException;

	/**
	 * Gets the Folder initializing the permissions.
	 * 
	 * @param folderId The folder identifier
	 * @param boolean True if the complete path must be retrieved
	 * @return The Folder bean
	 */
	public GUIFolder getFolder(long folderId, boolean computePath) throws ServerException;

	/**
	 * Deletes the folders and the subtrees
	 */
	public void delete(long[] folderIds) throws ServerException;

	/**
	 * Deletes a selection of folders from trash
	 */
	public void deleteFromTrash(Long[] ids) throws ServerException;

	/**
	 * Restores a given folder
	 */
	public void restore(Long[] folderIds, long parentId) throws ServerException;

	/**
	 * Moves some folders under a target folder
	 */
	public void move(long[] folderIds, long targetId) throws ServerException;

	/**
	 * Copies a folder under a target folder
	 */
	public void copyFolders(long[] folderIds, long targetId, boolean foldersOnly, boolean inheritSecurity)
			throws ServerException;

	/**
	 * Pastes documents into the target folder.
	 * 
	 * @param docIds The documents identifiers.
	 * @param folderId The target folder identifier.
	 * @param action The action selectee (Clipboard#COPY or Clipboard#COPY).
	 */
	public void paste(long[] docIds, long folderId, String action) throws ServerException;

	public void pasteAsAlias(long[] docIds, long folderId, String type) throws ServerException;

	/**
	 * Loads the folders templates
	 */
	public GUIValue[] loadTemplates() throws ServerException;

	/**
	 * Saves the passed folder templates
	 */
	public void saveTemplates(GUIValue[] templates) throws ServerException;

	/**
	 * Applies a template to a folder
	 */
	public void applyTemplate(long folderId, long templateId, boolean inheritSecurity) throws ServerException;

	public static class Instance {
		private static FolderServiceAsync instance;

		public static FolderServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(FolderService.class);
			}
			return instance;
		}
	}
}