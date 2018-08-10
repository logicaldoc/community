package com.logicaldoc.gui.common.client.observer;

import com.logicaldoc.gui.common.client.beans.GUIFolder;

/**
 * Listener on folders events
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public interface FolderObserver {
	/**
	 * Invoked when the user selects a new folder.
	 * 
	 * @param folder The newly selected folder
	 */
	public void onFolderSelected(GUIFolder folder);

	/**
	 * Invoked after the folder has been saved
	 * 
	 * @param folder The updated folder
	 */
	public void onFolderChanged(GUIFolder folder);

	/**
	 * Invoked after the folder has been deleted
	 * 
	 * @param folder The deleted folder
	 */
	public void onFolderDeleted(GUIFolder folder);

	/**
	 * Invoked after the folder has been created
	 * 
	 * @param folder The created folder
	 */
	public void onFolderCreated(GUIFolder folder);

	/**
	 * Invoked after the folder has been moved
	 * 
	 * @param folder The created folder
	 */
	public void onFolderMoved(GUIFolder folder);
}