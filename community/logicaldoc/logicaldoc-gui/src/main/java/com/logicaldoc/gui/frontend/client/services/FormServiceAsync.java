package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface FormServiceAsync {

	void create(GUIDocument form, AsyncCallback<GUIDocument> callback);

	void delete(long formId, AsyncCallback<Void> callback);
}
