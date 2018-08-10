package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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
	 */
	public void subscribeFolder(long folderId, boolean currentOnly, String[] events, Long user, Long group)
			throws ServerException;

	/**
	 * Subscribes a selection of documents
	 */
	public void subscribeDocuments(long[] docIds, String[] events, Long userId, Long groupId) throws ServerException;

	/**
	 * Changes a set of subscriptions
	 */
	public void update(long[] ids, boolean currentOnly, String[] events) throws ServerException;

	/**
	 * Deletes a list of Subscriptions
	 */
	public void deleteSubscriptions(long[] ids) throws ServerException;

	public static class Instance {
		private static AuditServiceAsync instance;

		public static AuditServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(AuditService.class);
			}
			return instance;
		}
	}
}