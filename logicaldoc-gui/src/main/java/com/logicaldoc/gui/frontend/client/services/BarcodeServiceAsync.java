package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeZone;

public interface BarcodeServiceAsync {

    void delete(long templateId, AsyncCallback<Void> callback);

    void save(GUIBarcodeTemplate template, AsyncCallback<GUIBarcodeTemplate> callback);

    void getTemplate(long templateId, AsyncCallback<GUIBarcodeTemplate> callback);

    void updateZone(GUIBarcodeZone zone, AsyncCallback<GUIBarcodeZone> callback);
}