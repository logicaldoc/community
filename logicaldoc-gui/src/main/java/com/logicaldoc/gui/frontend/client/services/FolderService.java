package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;

/**
 * The client side stub for the Folder Service. This service allows r/w
 * operations on folders.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("folder")
public interface FolderService extends RemoteService {
	/**
	 * Saves the folder in the DB
	 * 
	 * @param folder The folder to save
	 * 
	 * @return The saved folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFolder save(GUIFolder folder) throws ServerException;

	/**
	 * Creates a new folder
	 * 
	 * @param newFolder The folder to be created
	 * @param inheritSecurity True if the new folder has to inherit the security
	 *        policies from the parent
	 * 
	 * @return The saved folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFolder create(GUIFolder newFolder, boolean inheritSecurity) throws ServerException;

	/**
	 * Creates a new folder alias
	 * 
	 * @param parentId The folder in which the alias must be created
	 * @param foldRef The original folder
	 * 
	 * @return The created alias
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFolder createAlias(long parentId, long foldRef) throws ServerException;

	/**
	 * Renames the given folder
	 * 
	 * @param folderId identifier of the folder
	 * @param name the new folder's name
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void rename(long folderId, String name) throws ServerException;

	/**
	 * Applies all security settings to folder
	 * 
	 * @param folder The folder that contains the new security settings
	 * @param subfolders If true, the current security settings will be applied
	 *        to the sub-folders
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveACL(GUIFolder folder, boolean subfolders) throws ServerException;

	/**
	 * Inherits the ACL of another folder
	 * 
	 * @param folderId The folder that has to be updated
	 * @param rightsFolderId the folder that defines the rights
	 * 
	 * @return The updated Folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFolder inheritACL(long folderId, long rightsFolderId) throws ServerException;

	/**
	 * Applies all extended attributes to a sub-tree
	 * 
	 * @param parentId The parent folder containing the metadata
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyMetadata(long parentId) throws ServerException;

	/**
	 * Applies all tags to a sub-tree
	 * 
	 * @param parentId The parent folder containing the tags
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyTags(long parentId) throws ServerException;

	/**
	 * Applies the storage setting to a sub-tree
	 * 
	 * @param parentId The parent folder containing the tags
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyStorage(long parentId) throws ServerException;

	/**
	 * Applies all OCR settings to a sub-tree
	 * 
	 * @param parentId The parent folder containing the settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyOCR(long parentId) throws ServerException;

	/**
	 * Gets the Folder initializing the permissions.
	 * 
	 * @param folderId The folder identifier
	 * @param computePath True if the complete path must be retrieved
	 * @param computeDocs True if the number of contained documents must be
	 *        computed
	 * @param computeSubfolders True if the number of contained subfolders must
	 *        be computed
	 * 
	 * @return The Folder bean
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFolder getFolder(long folderId, boolean computePath, boolean computeDocs, boolean computeSubfolders)
			throws ServerException;

	/**
	 * Deletes the folders and the subtrees
	 * 
	 * @param folderIds identifiers of the folders
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long[] folderIds) throws ServerException;

	/**
	 * Deletes a selection of folders from trash
	 * 
	 * @param ids identifiers of the folders
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteFromTrash(Long[] ids) throws ServerException;

	/**
	 * Restores a given folder
	 * 
	 * @param folderIds identifiers of the folders
	 * @param targetId identifier of the folder that will receive the restore
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void restore(Long[] folderIds, long targetId) throws ServerException;

	/**
	 * Moves some folders under a target folder
	 * 
	 * @param folderIds identifiers of the folders
	 * @param targetId identifier of the folder that will receive the move
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void move(long[] folderIds, long targetId) throws ServerException;

	/**
	 * Merges some folders to a target folder
	 * 
	 * @param folderIds identifiers of the folders
	 * @param targetId identifier of the folder that will receive the merge
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void merge(long[] folderIds, long targetId) throws ServerException;

	/**
	 * Copies a folder under a target folder
	 * 
	 * @param folderIds identifiers of the folders
	 * @param targetId identifier of the folder that will receive the copy
	 * @param foldersOnly flag to copy just the folders and not the documents
	 * @param securityOption how to setup the security for the new folder'none',
	 *        'inherit' or 'replicate'
	 * @param model a model to use for creating the new folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void copyFolders(long[] folderIds, long targetId, boolean foldersOnly, String securityOption,
			GUIFolder model) throws ServerException;

	/**
	 * Pastes documents into the target folder
	 * 
	 * @param docIds the documents identifiers
	 * @param folderId the target folder identifier
	 * @param action the selected action({@link Clipboard#COPY} or
	 *        {@link Clipboard#COPY})
	 * @param links if the links must be copied too
	 * @param notes if the notes and annotations must be copied too
	 * @param security if the security settings must be copied too
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void paste(long[] docIds, long folderId, String action, boolean links, boolean notes, boolean security) throws ServerException;

	public void pasteAsAlias(long[] docIds, long folderId, String type) throws ServerException;

	/**
	 * Loads the folders templates
	 * 
	 * @return the folder templates
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIValue[] loadTemplates() throws ServerException;

	/**
	 * Saves the passed folder templates
	 * 
	 * @param templates the folder templates
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveTemplates(GUIValue[] templates) throws ServerException;

	/**
	 * Applies a template to a folder
	 * 
	 * @param folderId identifier of the folder to use as root
	 * @param templateId identifier of the template to use
	 * @param inheritSecurity if the new folders mus inherit the security from
	 *        <code>folderId</code>
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyTemplate(long folderId, long templateId, boolean inheritSecurity) throws ServerException;

	/**
	 * Counts the docs and subfolders of a given parent
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return the statistics (total number of documents, total number of subfolders, total size
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public long[] computeStats(long folderId) throws ServerException;

	/**
	 * Sets the pagination informations for the visualization of the folders
	 * tree
	 * 
	 * @param folderId ID of the folder
	 * @param startRecord Start rec index
	 * @param pageSize Current page size
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setFolderPagination(long folderId, Integer startRecord, Integer pageSize) throws ServerException;

	/**
	 * Replicates the folder's grid layout to all the subtree
	 * 
	 * @param folderId identifier of the root folder whose setting will be
	 *        replicated
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void applyGridLayout(long folderId) throws ServerException;

	
	/**
	 * Read the uploaded image and converts it into Base64 
	 * 
	 * @return the string conversion of the uploaded image
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String readImage() throws ServerException;
	
	public static class Instance {
		private static FolderServiceAsync inst;

		private Instance() {
		}
		
		public static FolderServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(FolderService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}