package com.logicaldoc.gui.frontend.client.whatsapp;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface WhatsappServiceAsync {

    public void loadSettings(AsyncCallback<List<String>> callback);

    public void saveSettings(List<String> settings, AsyncCallback<Void> callback);
}