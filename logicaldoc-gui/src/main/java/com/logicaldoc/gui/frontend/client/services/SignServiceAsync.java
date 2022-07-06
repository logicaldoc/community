package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIKeystore;

public interface SignServiceAsync {

	void loadKeystore(long tenantId, AsyncCallback<GUIKeystore> callback);

	void saveKeystore(GUIKeystore keystore, AsyncCallback<Void> callback);

	void generateNewKeystore(GUIKeystore keystore, AsyncCallback<Void> callback);

	void deleteKeystore(long tenantId, AsyncCallback<Void> callback);

	void imporKeystore(GUIKeystore keystore, AsyncCallback<Void> callback);

	void generateNewCertificate(AsyncCallback<Void> callback);

	void deleteCertificate(AsyncCallback<Void> callback);

	void signDocuments(long[] docIds, String reason, int page, String signX, String signY, String signWidth,
			AsyncCallback<Void> callback);

	void isVisualSignatureEnabled(AsyncCallback<Boolean> callback);
}