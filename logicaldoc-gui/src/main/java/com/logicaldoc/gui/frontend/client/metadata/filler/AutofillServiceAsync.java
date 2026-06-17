package com.logicaldoc.gui.frontend.client.metadata.filler;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface AutofillServiceAsync {

	void deleteFillers(List<Long> fillerIds, AsyncCallback<Void> callback);

	void saveFiller(GUIFiller filler, AsyncCallback<GUIFiller> callback);

	void getFiller(long fillerId, AsyncCallback<GUIFiller> callback);

	void fillTags(GUIDocument document, AsyncCallback<GUIDocument> callback);

	void fillLanguage(GUIDocument document, AsyncCallback<GUIDocument> callback);

	void fillTemplate(GUIDocument document, AsyncCallback<GUIDocument> callback);

	void fill(GUIDocument document, long fillerId, boolean explain, AsyncCallback<GUIDocument> callback);

	void testRegex(String sample, String regex, boolean inclusive, AsyncCallback<String> callback);
	
	void rescheduleAll(AsyncCallback<Void> callback);

    void markUnprocessable(List<Long> ids, AsyncCallback<Void> asyncCallback);
}