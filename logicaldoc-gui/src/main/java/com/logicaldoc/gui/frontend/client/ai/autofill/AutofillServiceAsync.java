package com.logicaldoc.gui.frontend.client.ai.autofill;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface AutofillServiceAsync {

	void deleteFillers(List<Long> fillerIds, AsyncCallback<Void> callback);

	void saveFiller(GUIFiller filler, AsyncCallback<GUIFiller> callback);

	void getFiller(long fillerId, AsyncCallback<GUIFiller> callback);

	void fillTags(GUIDocument document, AsyncCallback<GUIDocument> callback);
}