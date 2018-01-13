package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.beans.GUIIncrementalArchive;

/**
 * The client side stub for the Impex Service. This service allows r/w
 * operations on export archives.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
@RemoteServiceRelativePath("impex")
public interface ImpexService extends RemoteService {
	/**
	 * Deletes a specific archive by its ID
	 */
	public void delete(long archiveId) throws ServerException;

	/**
	 * Deletes a set of versions from the given archive
	 */
	public GUIArchive deleteVersions(long archiveId, Long versionIds[]) throws ServerException;

	/**
	 * Change the status of the given Archive
	 */
	public void setStatus(long archiveId, int status) throws ServerException;

	/**
	 * Saves/Updates a given archive
	 */
	public GUIArchive save(GUIArchive archive) throws ServerException;

	/**
	 * Loads a given archive
	 */
	public GUIArchive load(long archiveId) throws ServerException;

	/**
	 * Adds a set of documents(their current versions) to the given archive
	 */
	public void addDocuments(long archiveId, long[] documentIds) throws ServerException;

	/**
	 * Adds a a folder(the current version of the contained documents at any
	 * level).
	 */
	public void addFolder(long archiveId, long rootId) throws ServerException;

	/**
	 * Deletes a given incremental configuration
	 */
	public void deleteIncremental(long id) throws ServerException;

	/**
	 * Loads an incremental configuration
	 */
	public GUIIncrementalArchive loadIncremental(long id) throws ServerException;

	/**
	 * Saves the passed incremental configuration
	 */
	public GUIIncrementalArchive saveIncremental(GUIIncrementalArchive incremental) throws ServerException;

	/**
	 * Deletes a folder in impex/in folder.
	 */
	public void deleteFolder(String folderName) throws ServerException;

	/**
	 * Create a new import archive for the specified bundle folder
	 */
	public void startImport(String folderName) throws ServerException;

	public static class Instance {
		private static ImpexServiceAsync instance;

		public static ImpexServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ImpexService.class);
			}
			return instance;
		}
	}
}