package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;

public interface MessageServiceAsync {

	void delete(List<Long> ids, AsyncCallback<Void> callback);

	void getMessage(long messageId, boolean markAsRead, AsyncCallback<GUIMessage> callback);

	void save(GUIMessage message, List<Long> recipientIds, AsyncCallback<Void> callback);

	void loadTemplates(String language, String type, AsyncCallback<List<GUIMessageTemplate>> callback);

	void saveTemplates(List<GUIMessageTemplate> templates, AsyncCallback<Void> callback);

	void deleteTemplates(String name, AsyncCallback<Void> callback);

	void deleteTemplates(List<Long> ids, AsyncCallback<Void> callback);

	void getTemplate(long templateId, AsyncCallback<GUIMessageTemplate> callback);
}