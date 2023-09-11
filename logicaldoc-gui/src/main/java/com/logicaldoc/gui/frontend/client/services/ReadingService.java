package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIReading;

/**
 * The client side stub for the Reading Service. This service allows the reading
 * requests / confirmations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
@RemoteServiceRelativePath("reading")
public interface ReadingService extends RemoteService {

	/**
	 * Asks some users to confirm the reading of a set of documents
	 * 
	 * @param docIds identifiers of the documents
	 * @param recipientIds identifiers of the recipients
	 * @param comment an optional message to include in the notification
	 * @param alertConfirmation if the requestor must be notified on reading confirmation
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void askReadingConfirmation(Long[] docIds, long[] recipientIds, boolean alertConfirmation, String comment) throws ServerException;

	/**
	 * Confirms the read completion of a given file version
	 * 
	 * @param readingIds identifiers of the readings to confirm, all must refer to the same document
	 * @param version the version
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void confirmReadings(long[] readingIds, String version) throws ServerException;
	
	/**
	 * Retrieves all the unconfirmed readings by the current user
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIReading[] getUnconfimedReadings() throws ServerException;

	public static class Instance {
		private static ReadingServiceAsync inst;

		private Instance() {
		}

		public static ReadingServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ReadingService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}