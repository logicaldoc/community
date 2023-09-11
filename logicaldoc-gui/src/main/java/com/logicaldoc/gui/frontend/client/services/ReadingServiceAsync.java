package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIReading;

public interface ReadingServiceAsync {

	void askReadingConfirmation(Long[] docIds, long[] recipientIds, boolean alertConfirmation, String comment, AsyncCallback<Void> callback);

	void confirmReadings(long[] readingIds, String version, AsyncCallback<Void> callback);

	void getUnconfimedReadings(AsyncCallback<GUIReading[]> callback);

}