package com.logicaldoc.gui.common.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
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

	/**
	 * Retrieves all the settings for the current session
	 * 
	 * @return array with all the settings
	 * 
	 * @throws InvalidSessionServerException the session does not exist or is expired
	 */
	public GUIParameter[] getSessionInfo() throws InvalidSessionServerException;

	/**
	 * Retrieves the natural language description of a given cron expression
	 * 
	 * @param expression the cron expression to evaluate
	 * @param locale the locale to use for the description
	 * 
	 * @return the natural language description
	 * 
	 * @throws ServerException raised in case the given expression is invalid
	 */
	public String getCronDescription(String expression, String locale) throws ServerException;

	/**
	 * Ping to maintain open he session
	 * 
	 * @return if the server has been successfully contacted
	 * 
	 * @throws InvalidSessionServerException the session does not exist or is expired
	 */
	public boolean ping() throws InvalidSessionServerException;

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