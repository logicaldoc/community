package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIRetentionPolicy;

/**
 * The client side stub for the RetentionPolicies Service. This service gives
 * all needed methods to handle the retention policies.
 */
@RemoteServiceRelativePath("retentionpolicies")
public interface RetentionPoliciesService extends RemoteService {
	/**
	 * Deletes a given policy
	 * 
	 * @param id identifier of the retention policy
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates a retention policy
	 * 
	 * @param policy the retention policy to save
	 * 
	 * @return the saved retention policy
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRetentionPolicy save(GUIRetentionPolicy policy) throws ServerException;

	/**
	 * Loads a given policy from the database
	 * 
	 * @param id identifiers of the policy
	 * 
	 * @return the saved retention policy
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRetentionPolicy getPolicy(long id) throws ServerException;

	/**
	 * Reorder the policies
	 * 
	 * @param ids identifier of the policies to reorder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void reorder(long[] ids) throws ServerException;

	/**
	 * Changes a policy enabled/disabled status
	 * 
	 * @param id identifier of the policy to enable/disable
	 * @param enabled the enabled status of the policy
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	public static class Instance {
		private static RetentionPoliciesServiceAsync instance;

		public static RetentionPoliciesServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(RetentionPoliciesService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}