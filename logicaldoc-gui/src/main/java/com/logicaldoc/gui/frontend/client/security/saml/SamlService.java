package com.logicaldoc.gui.frontend.client.security.saml;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;

/**
 * Service for Saml administration
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
@RemoteServiceRelativePath("saml")
public interface SamlService extends RemoteService {

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