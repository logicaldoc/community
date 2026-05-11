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
     * Tests the connection to Whatsapp
     * 
     * @return if the connection has been established
     * 
     * @throws ServerException an error happened in the server application
     */
    public boolean testConnection() throws ServerException;
    
    /**
     * Retrieve the settings saved for connecting to Whatsapp
     * 
     * @param settings the settings
     * 
     * @throws ServerException an error happened in the server application
     */
    public void saveSettings(List<String> settings) throws ServerException;

    /**
     * Activates the number in order to use with the API
     * 
     * @param pin The PIN received by Whatsapp when you activates the number the
     *        first time
     * 
     * @return if the activation ended succesfully
     * 
     * @throws ServerException an error happened in the server application
     */
    public boolean activateNumber(String pin) throws ServerException;

    /**
     * Prepares all required templates in the Whatsapp account
     * 
     * @return A result entry per template
     * 
     * @throws ServerException an error happened in the server application
     */
    public List<TemplateResult> prepareTemplates() throws ServerException;

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