package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.beans.GUIIncrementalArchive;

/**
 * The client side stub for the Impex Service. This service allows r/w
 * operations on export archives.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("impex")
public interface ImpexService extends RemoteService {
	/**
	 * Deletes a specific archive by its identifier
	 * 
	 * @param archiveId identifier of the archive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long archiveId) throws ServerException;

	/**
	 * Deletes a set of versions from the given archive
	 * 
	 * @param archiveId identifier of the archive
	 * @param versionIds identifiers od the versions
	 * 
	 * @return the archive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIArchive deleteVersions(long archiveId, Long versionIds[]) throws ServerException;

	/**
	 * Change the status of the given Archive
	 * 
	 * @param archiveId identifier of the archive 
	 * @param status the status of the archive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setStatus(long archiveId, int status) throws ServerException;

	/**
	 * Saves/Updates a given archive
	 * 
	 * @param archive the archive to save
	 * 
	 * @return the saved archive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIArchive save(GUIArchive archive) throws ServerException;

	/**
	 * Loads a given archive
	 * 
	 * @param archiveId identifier of the archive 
	 * 
	 * @return the archive retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIArchive load(long archiveId) throws ServerException;

	/**
	 * Adds a set of documents(their current versions) to the given archive
	 * 
	 * @param archiveId identifier of the archive 
	 * @param documentIds identifiers of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void addDocuments(long archiveId, long[] documentIds) throws ServerException;

	/**
	 * Adds a a folder(the current version of the contained documents at any
	 * level)
	 * 
	 * @param archiveId identifier of the archive
	 * @param folderId identifier od the folder to add
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void addFolder(long archiveId, long folderId) throws ServerException;

	/**
	 * Deletes a given incremental configuration
	 * 
	 * @param id identifier of the incremental configuration
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteIncremental(long id) throws ServerException;

	/**
	 * Loads an incremental configuration
	 * 
	 * @param id identifier of the incremental configuration 
	 * 
	 * @return the configuration retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIIncrementalArchive loadIncremental(long id) throws ServerException;

	/**
	 * Saves the passed incremental configuration
	 * 
	 * @param incremental the configuration to save
	 * 
	 * @return the saved configuration
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIIncrementalArchive saveIncremental(GUIIncrementalArchive incremental) throws ServerException;

	/**
	 * Deletes a folder in impex/in folder
	 * 
	 * @param folderName name of the sub-folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteFolder(String folderName) throws ServerException;

	/**
	 * Create a new import archive for the specified bundle folder
	 * 
	 * @param folderName name of the sub-folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void startImport(String folderName) throws ServerException;

	public static class Instance {
		private static ImpexServiceAsync instance;

		public static ImpexServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ImpexService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}