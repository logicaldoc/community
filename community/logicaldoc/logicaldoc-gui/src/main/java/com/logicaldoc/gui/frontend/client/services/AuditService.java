package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Audit Service. This service allows folders and
 * documents subscription.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("audit")
public interface AuditService extends RemoteService {

	/**
	 * Subscribes a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param currentOnly if the subscription must be on the folder only(not
	 *        children)
	 * @param events the events to subscribe to
	 * @param userId identifier of the user (if not specified, fill the
	 *        <code>groupId</code>)
	 * @param groupId identifier of the group (if not specified, fill the
	 *        <code>userId</code>)
	 * 
	 * @throws ServerException error in the server application
	 */
	public void subscribeFolder(long folderId, boolean currentOnly, String[] events, Long userId, Long groupId)
			throws ServerException;

	/**
	 * Subscribes a selection of documents
	 * 
	 * @param docIds identifiers of the documents
	 * @param events the events to subscribe to
	 * @param userId identifier of the user (if not specified, fill the
	 *        <code>groupId</code>)
	 * @param groupId identifier of the group (if not specified, fill the
	 *        <code>userId</code>)
	 * 
	 * @throws ServerException error in the server application
	 */
	public void subscribeDocuments(long[] docIds, String[] events, Long userId, Long groupId) throws ServerException;

	/**
	 * Changes a set of subscriptions
	 * 
	 * @param ids identifiers of the subscriptions
	 * @param currentOnly if the subscription must be on the folder only(not
	 *        children)
	 * @param events the events to subscribe to
	 * 
	 * @throws ServerException error in the server application
	 */
	public void update(long[] ids, boolean currentOnly, String[] events) throws ServerException;

	/**
	 * Deletes a list of Subscriptions
	 * 
	 * @param ids identifiers of the subscriptions
	 * 
	 * @throws ServerException error in the server application
	 */
	public void deleteSubscriptions(long[] ids) throws ServerException;

	public static class Instance {
		private static AuditServiceAsync instance;

		public static AuditServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(AuditService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}