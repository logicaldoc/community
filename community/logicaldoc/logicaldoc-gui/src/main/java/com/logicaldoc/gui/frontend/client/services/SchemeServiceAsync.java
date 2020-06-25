package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIScheme;
import com.logicaldoc.gui.common.client.beans.GUISequence;

public interface SchemeServiceAsync {

	void delete(long templateId, String type, AsyncCallback<Void> callback);

	void get(long templateId, String type, AsyncCallback<GUIScheme> callback);

	void load(AsyncCallback<GUIScheme[]> callback);

	void save(GUIScheme customid, AsyncCallback<Void> callback);

	void resetSequence(long sequenceId, long value, AsyncCallback<Void> callback);

	void loadSequences(AsyncCallback<GUISequence[]> callback);

	void deleteSequence(long sequenceId, AsyncCallback<Void> callback);

}
