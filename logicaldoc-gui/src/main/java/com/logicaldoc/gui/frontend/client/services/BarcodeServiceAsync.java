package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeZone;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface BarcodeServiceAsync {
	void rescheduleAll(AsyncCallback<Void> callback);

	void markUnprocessable(List<Long> ids, AsyncCallback<Void> asyncCallback);

	void delete(long templateId, AsyncCallback<Void> callback);

	void save(GUIBarcodeTemplate template, AsyncCallback<GUIBarcodeTemplate> callback);

	void getTemplate(long templateId, AsyncCallback<GUIBarcodeTemplate> callback);

	void process(long docId, AsyncCallback<GUIDocument> callback);

	void updateZone(GUIBarcodeZone zone, AsyncCallback<GUIBarcodeZone> callback);
}