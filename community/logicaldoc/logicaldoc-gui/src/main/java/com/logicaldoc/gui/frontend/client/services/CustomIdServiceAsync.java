package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUICustomId;
import com.logicaldoc.gui.common.client.beans.GUISequence;

public interface CustomIdServiceAsync {

	void delete(long templateId, String type, AsyncCallback<Void> callback);

	void get(long templateId, String type, AsyncCallback<GUICustomId> callback);

	void load(AsyncCallback<GUICustomId[]> callback);

	void save(GUICustomId customid, AsyncCallback<Void> callback);

	void resetSequence(long sequenceId, long value, AsyncCallback<Void> callback);

	void loadSequences(AsyncCallback<GUISequence[]> callback);

	void deleteSequence(long sequenceId, AsyncCallback<Void> callback);

}
