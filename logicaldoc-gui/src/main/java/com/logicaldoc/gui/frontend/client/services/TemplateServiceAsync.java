package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIExtensibleObject;
import com.logicaldoc.gui.common.client.beans.GUITemplate;

public interface TemplateServiceAsync {

	void delete(long templateId, AsyncCallback<Void> callback);

	void save(GUITemplate template, AsyncCallback<GUITemplate> callback);

	void clone(long templateId, String cloneName, AsyncCallback<GUITemplate> callback);

	void getTemplate(long templateId, AsyncCallback<GUITemplate> callback);

	void countDocuments(long templateId, AsyncCallback<Long> callback);

	void getAttributes(long templateId, GUIExtensibleObject extensibleObject,
			AsyncCallback<List<GUIAttribute>> callback);
}