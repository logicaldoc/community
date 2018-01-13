package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Annotations Service.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.2
 */
@RemoteServiceRelativePath("annotations")
public interface AnnotationsService extends RemoteService {

	/**
	 * Prepares the system to accept annotations for the given document
	 * 
	 * @param docId The document to elaborate
	 * @param fileVersion Optional file version specification
	 * @return The number of document's pages
	 * @throws ServerException
	 */
	public int prepareAnnotations(long docId, String fileVersion) throws ServerException;

	/**
	 * Adds a new annotation in the given page
	 * 
	 * @param docId The document identifier
	 * @param page The current page
	 * @param snippet A snippet of the selected text
	 * @param text The annotation's text
	 * 
	 * @return The new annotation's ID
	 * @throws ServerException
	 */
	public long addAnnotation(long docId, int page, String snippet, String text) throws ServerException;

	/**
	 * Saves the page with annotations
	 * 
	 * @param docId The document identifier
	 * @param page The current page
	 * @param content The page's content
	 * 
	 * @throws ServerException
	 */
	public void savePage(long docId, int page, String content) throws ServerException;

	public static class Instance {
		private static AnnotationsServiceAsync instance;

		public static AnnotationsServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(AnnotationsService.class);
			}
			return instance;
		}
	}
}