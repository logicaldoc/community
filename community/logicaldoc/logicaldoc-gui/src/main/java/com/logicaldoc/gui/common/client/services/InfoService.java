package com.logicaldoc.gui.common.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * Informations service
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0.0
 */
@RemoteServiceRelativePath("info")
public interface InfoService extends RemoteService {
	
	/**
	 * Retrieves the system informations
	 * 
	 * @param locale language to use
	 * @param tenant name of the tenant
	 * @param login if the informations are asked by the login form
	 * 
	 * @return the User Inteface's informations
	 */
	public GUIInfo getInfo(String locale, String tenant, boolean login);

	public GUIParameter[] getSessionInfo() throws InvalidSessionException;

	/**
	 * Ping to maintain open he session
	 * 
	 * @return if the server has been successfully contacted 
	 * 
	 * @throws InvalidSessionException the session does not exist or is expired
	 */
	public boolean ping() throws InvalidSessionException;

	public static class Instance {
		private static InfoServiceAsync instance;

		public static InfoServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(InfoService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}