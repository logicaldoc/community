package com.logicaldoc.gui.frontend.client.whatsapp;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Whatsapp Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
@RemoteServiceRelativePath("whatsapp")
public interface WhatsappService extends RemoteService {

    /**
     * Retrieve the settings saved for connecting to Whatsapp
     * 
     * @return the settings
     * 
     * @throws ServerException an error happened in the server application
     */
    public List<String> loadSettings() throws ServerException;

    /**
     * Retrieve the settings saved for connecting to Whatsapp
     * 
     * @param settings the settings
     * 
     * @throws ServerException an error happened in the server application
     */
    public void saveSettings(List<String> settings) throws ServerException;

    public static class Instance {
        private static WhatsappServiceAsync inst;

        private Instance() {
        }

        public static WhatsappServiceAsync get() {
            if (inst == null) {
                inst = GWT.create(WhatsappService.class);
                ((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
            }
            return inst;
        }
    }
}