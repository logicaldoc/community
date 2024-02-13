package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;

public interface ReadingRequestServiceAsync {

	void askReadingConfirmation(List<Long> docIds, List<Long> userIds, List<Long> groupIds, boolean alertConfirmation,
			String comment, AsyncCallback<Void> callback);

	void confirmReadings(List<Long> readingIds, String version, AsyncCallback<Void> callback);

	void getUnconfimedReadings(AsyncCallback<List<GUIReadingRequest>> callback);

	void delete(long readingId, AsyncCallback<Void> callback);

	void notityReadingRequest(long readingId, AsyncCallback<Void> callback);
}