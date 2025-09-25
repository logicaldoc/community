package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

public interface OCRServiceAsync {

	void loadSettings(AsyncCallback<List<GUIParameter>> callback);
}