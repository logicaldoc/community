package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
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
	 * 
	 * @return the tag cloud
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITag[] getTagCloud() throws ServerException;

	/**
	 * Deletes an existing tag
	 * 
	 * @param tag the tag to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(String tag) throws ServerException;

	/**
	 * Rename an existing tag to another label
	 * 
	 * @param tag the actual tag
	 * @param newTag the new tag
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void rename(String tag, String newTag) throws ServerException;

	/**
	 * Adds a new tag in the list of available tags
	 * 
	 * @param tag the tag to add
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void addTag(String tag) throws ServerException;

	/**
	 * Removes an available from the list of available tags
	 * 
	 * @param tag the tag to remove
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void removeTag(String tag) throws ServerException;

	/**
	 * Gets the tag settings
	 * 
	 * @return tag settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIParameter[] getSettings() throws ServerException;

	public static class Instance {
		private static TagServiceAsync inst;

		private Instance() {
		}
		
		public static TagServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(TagService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}