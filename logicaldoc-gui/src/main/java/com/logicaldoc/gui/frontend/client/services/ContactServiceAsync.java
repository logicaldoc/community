package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIParseContactsParameters;

public interface ContactServiceAsync {

	void delete(List<Long> ids, AsyncCallback<Void> callback);

	void load(long id, AsyncCallback<GUIContact> callback);

	void save(GUIContact contact, AsyncCallback<Void> callback);

	void parseContacts(boolean preview, GUIParseContactsParameters parameters, AsyncCallback<List<GUIContact>> callback);

	void shareContacts(List<Long> contactIds, List<Long> userIds, List<Long> groupIds, AsyncCallback<Void> callback);
}