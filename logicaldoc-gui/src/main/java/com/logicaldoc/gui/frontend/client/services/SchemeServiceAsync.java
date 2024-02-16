package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIScheme;
import com.logicaldoc.gui.common.client.beans.GUISequence;

public interface SchemeServiceAsync {

	void delete(long templateId, String type, AsyncCallback<Void> callback);

	void get(long templateId, String type, AsyncCallback<GUIScheme> callback);

	void load(AsyncCallback<List<GUIScheme>> callback);

	void save(GUIScheme customid, AsyncCallback<Void> callback);

	void resetSequence(long sequenceId, long value, AsyncCallback<Void> callback);

	void loadSequences(AsyncCallback<List<GUISequence>> callback);

	void deleteSequence(long sequenceId, AsyncCallback<Void> callback);

}
