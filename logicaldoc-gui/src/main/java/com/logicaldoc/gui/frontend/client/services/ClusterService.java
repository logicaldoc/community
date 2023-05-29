package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the ClusterService Service. This service gives
 * access to the Clustering stuff.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
@RemoteServiceRelativePath("cluster")
public interface ClusterService extends RemoteService {

	/**
	 * Shares a list of configuration parameters
	 * 
	 * @param parameters the parameters to make global
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void makeGlobal(String[] parameters) throws ServerException;

	/**
	 * Unshares a list of configuration parameters
	 * 
	 * @param parameters the parameters to make local
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void makeLocal(String[] parameters) throws ServerException;

	public static class Instance {
		private static ClusterServiceAsync inst;

		private Instance() {
		}

		public static ClusterServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ClusterService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}