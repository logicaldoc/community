package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITag;

public interface TagServiceAsync {
	void delete(String tag, AsyncCallback<Void> callback);

	void getTagCloud(AsyncCallback<List<GUITag>> callback);

	void rename(String tag, String newTag, AsyncCallback<Void> callback);

	void addTag(String tag, AsyncCallback<Void> callback);

	void removeTag(String tag, AsyncCallback<Void> callback);

	void getSettings(AsyncCallback<List<GUIParameter>> callback);
}