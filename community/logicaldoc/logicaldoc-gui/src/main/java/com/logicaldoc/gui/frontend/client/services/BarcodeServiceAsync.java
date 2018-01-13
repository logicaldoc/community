package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeEngine;
import com.logicaldoc.gui.common.client.beans.GUIBarcodePattern;

public interface BarcodeServiceAsync {

	void getInfo(AsyncCallback<GUIBarcodeEngine> callback);

	void save(GUIBarcodeEngine engine, AsyncCallback<Void> callback);

	void rescheduleAll(AsyncCallback<Void> callback);

	void markUnprocessable(long[] ids, AsyncCallback<Void> asyncCallback);

	void loadPatterns(Long templateId, AsyncCallback<GUIBarcodePattern[]> callback);

	void savePatterns(String[] patterns, Long templateId, AsyncCallback<Void> callback);

}