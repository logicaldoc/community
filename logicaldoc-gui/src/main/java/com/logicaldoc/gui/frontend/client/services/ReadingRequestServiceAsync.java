package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;

public interface ReadingRequestServiceAsync {

	void askReadingConfirmation(Long[] docIds, long[] userIds, long[] groupIds, boolean alertConfirmation,
			String comment, AsyncCallback<Void> callback);

	void confirmReadings(long[] readingIds, String version, AsyncCallback<Void> callback);

	void getUnconfimedReadings(AsyncCallback<GUIReadingRequest[]> callback);

	void delete(long readingId, AsyncCallback<Void> callback);

	void notityReadingRequest(long readingId, AsyncCallback<Void> callback);
}