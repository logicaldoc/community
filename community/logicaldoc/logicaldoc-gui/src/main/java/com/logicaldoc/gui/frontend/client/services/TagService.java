package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITag;

/**
 * Tag handling service
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("tag")
public interface TagService extends RemoteService {

	/**
	 * Loads the tag cloud from the server
	 */
	public GUITag[] getTagCloud() throws ServerException;

	/**
	 * Deletes an existing tag
	 */
	public void delete(String tag) throws ServerException;

	/**
	 * Rename an existing tag to another label
	 */
	public void rename(String tag, String newTag) throws ServerException;

	/**
	 * Adds a new tag in the list of available tags.
	 */
	public void addTag(String tag) throws ServerException;

	/**
	 * Removes an available from the list of available tags.
	 */
	public void removeTag(String tag) throws ServerException;

	/**
	 * Gets the tag settings
	 */
	public GUIParameter[] getSettings() throws ServerException;

	public static class Instance {
		private static TagServiceAsync instance;

		public static TagServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(TagService.class);
			}
			return instance;
		}
	}
}