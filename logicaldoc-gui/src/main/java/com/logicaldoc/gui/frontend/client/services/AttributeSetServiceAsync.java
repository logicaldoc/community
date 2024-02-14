package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface AttributeSetServiceAsync {

	void delete(long setId, AsyncCallback<Void> callback);

	void deleteOptions(long setId, String attribute, List<String> values, AsyncCallback<Void> callback);

	void getAttributeSet(long setId, AsyncCallback<GUIAttributeSet> callback);

	void parseOptions(long setId, String attribute, AsyncCallback<List<GUIValue>> callback);

	void save(GUIAttributeSet set, AsyncCallback<GUIAttributeSet> callback);

	void saveOptions(long setId, String attribute, List<GUIValue> options, AsyncCallback<Void> callback);

	void getAttributeSets(AsyncCallback<List<GUIAttributeSet>> callback);

	void applyAllToTemplates(long setId, String attribute, AsyncCallback<Void> callback);

	void applyValidationToTemplates(long setId, String attribute, AsyncCallback<Void> callback);

	void applyInitializationToTemplates(long setId, String attribute, AsyncCallback<Void> callback);
}