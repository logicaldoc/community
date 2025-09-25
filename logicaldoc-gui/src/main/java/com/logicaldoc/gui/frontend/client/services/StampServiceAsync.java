package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIStamp;

public interface StampServiceAsync {

	void delete(long id, AsyncCallback<Void> callback);

	void save(GUIStamp stamp, AsyncCallback<GUIStamp> callback);

	void getStamp(long id, AsyncCallback<GUIStamp> callback);

	void getStamp(String name, AsyncCallback<GUIStamp> callback);

	void changeStatus(long id, boolean enabled, AsyncCallback<Void> callback);

	void saveImage(long stampId, AsyncCallback<Void> callback);

	void applyStamp(List<Long> docIds, GUIStamp stamp, AsyncCallback<Void> callback);

	void getSignature(AsyncCallback<GUIStamp> callback);
}