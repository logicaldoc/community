package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;

public interface MessageServiceAsync {

	void delete(long[] ids, AsyncCallback<Void> callback);

	void getMessage(long messageId, boolean markAsRead, AsyncCallback<GUIMessage> callback);

	void save(GUIMessage message, long[] recipientIds, AsyncCallback<Void> callback);

	void loadTemplates(String language, String type, AsyncCallback<GUIMessageTemplate[]> callback);

	void saveTemplates(GUIMessageTemplate[] templates, AsyncCallback<Void> callback);

	void deleteTemplates(String name, AsyncCallback<Void> callback);

	void deleteTemplates(long[] ids, AsyncCallback<Void> callback);

	void getTemplate(long templateId, AsyncCallback<GUIMessageTemplate> callback);
}