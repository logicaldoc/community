package com.logicaldoc.core.folder;

import java.util.Map;

import com.logicaldoc.core.PersistenceException;

/**
 * This interface defines hooks called before and after a particular event
 * occurs on the specified folder.
 * <p>
 * Each methods has access to a dictionary map that can be used through the
 * execution pipeline in order to carry needed informations among all listeners.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.4
 */
public interface FolderListener {
	
	/**
	 * Called before a folder is stored in the database
	 * 
	 * @param folder The folder to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void beforeStore(Folder folder, FolderHistory transaction, Map<String, Object> dictionary) throws PersistenceException;

	/**
	 * Called after a folder is stored in the database
	 * 
	 * @param folder The folder to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void afterStore(Folder folder, FolderHistory transaction, Map<String, Object> dictionary) throws PersistenceException;
}