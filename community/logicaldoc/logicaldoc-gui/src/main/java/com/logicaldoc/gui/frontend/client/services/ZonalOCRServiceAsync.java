package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIOCRTemplate;
import com.logicaldoc.gui.common.client.beans.GUIZone;

public interface ZonalOCRServiceAsync {

	void delete(long templateId, AsyncCallback<Void> callback);

	void save(GUIOCRTemplate template, AsyncCallback<GUIOCRTemplate> callback);

	void getTemplate(long templateId, AsyncCallback<GUIOCRTemplate> callback);

	void updateZone(GUIZone zone, AsyncCallback<GUIZone> callback);

	void process(long docId, AsyncCallback<GUIDocument> callback);

	void rescheduleAll(AsyncCallback<Void> callback);

	void markUnprocessable(long[] ids, AsyncCallback<Void> callback);
}