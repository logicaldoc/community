package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;

public interface AttributeSetServiceAsync {

	void delete(long setId, AsyncCallback<Void> callback);

	void deleteOptions(long setId, String attribute, String[] values, AsyncCallback<Void> callback);

	void getAttributeSet(long setId, AsyncCallback<GUIAttributeSet> callback);

	void parseOptions(long setId, String attribute, AsyncCallback<String[]> callback);

	void save(GUIAttributeSet set, AsyncCallback<GUIAttributeSet> callback);

	void saveOptions(long setId, String attribute, String[] values, AsyncCallback<Void> callback);
}
