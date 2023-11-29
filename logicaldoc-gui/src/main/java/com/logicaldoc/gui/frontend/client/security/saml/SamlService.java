package com.logicaldoc.gui.frontend.client.security.saml;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * Service for Saml administration
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
@RemoteServiceRelativePath("saml")
public interface SamlService extends RemoteService {

	public GUISamlSettings loadSettings() throws ServerException;

	public void saveSettings(GUISamlSettings settings) throws ServerException;
	
	/**
	 * Take the last uploaded file and imports it in the SAML settings
	 * 
	 * @param resourceName Name of the resource to import (certificate, privatekey, idpmetadata)
	 * 
	 * @throws ServerException Error happened in the server
	 * 
	 * @return the resource's content
	 */
	public String importResource(String resourceName) throws ServerException;
	
	public static class Instance {
		private static SamlServiceAsync inst;

		private Instance() {
		}

		public static SamlServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SamlService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}