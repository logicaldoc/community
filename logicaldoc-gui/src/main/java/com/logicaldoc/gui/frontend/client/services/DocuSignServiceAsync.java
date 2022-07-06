package com.logicaldoc.gui.frontend.client.services;

import java.util.Collection;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocuSignSettings;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

public interface DocuSignServiceAsync {

	void loadSettings(AsyncCallback<GUIDocuSignSettings> callback);

	void authorize(GUIDocuSignSettings settings, AsyncCallback<String> callback);

	void isAuthorized(AsyncCallback<Boolean> callback);

	void getSigners(String envelopeId, AsyncCallback<Collection<String>> callback);

	void validateEnvelope(Collection<Long> docIds, AsyncCallback<Collection<GUIDocument>> callback);

	void sendEnvelope(GUIDocuSignSettings envelope, AsyncCallback<String> callback);
}