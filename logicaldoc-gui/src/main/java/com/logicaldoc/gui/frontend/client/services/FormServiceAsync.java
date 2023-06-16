package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIForm;

public interface FormServiceAsync {

	void save(GUIForm form, AsyncCallback<GUIForm> callback);

	void delete(long formId, AsyncCallback<Void> callback);

	void getById(long id, AsyncCallback<GUIForm> callback);

	void processImage(AsyncCallback<String> callback);

	void invite(GUIForm form, GUIEmail email, String locale, AsyncCallback<Void> callback);

	void getPreFilledLink(GUIForm form, String responderEmail, AsyncCallback<String> callback);
}
