package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.ParseContactsParameters;

public interface ContactServiceAsync {

	void delete(long[] ids, AsyncCallback<Void> callback);

	void load(long id, AsyncCallback<GUIContact> callback);

	void save(GUIContact contact, AsyncCallback<Void> callback);

	void parseContacts(boolean preview, ParseContactsParameters parameters, AsyncCallback<GUIContact[]> callback);

	void shareContacts(long[] contactIds, long[] userIds, long[] groupIds, AsyncCallback<Void> callback);
}