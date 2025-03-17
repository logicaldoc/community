package com.logicaldoc.gui.frontend.client.ai;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.frontend.client.ai.sampler.GUISampler;

/**
 * The client side stub for the Artificial Intelligence Service. This service
 * gives all needed methods to handle templates.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
@RemoteServiceRelativePath("ai")
public interface AIService extends RemoteService {
	/**
	 * Deletes a given sampler
	 * 
	 * @param samplerId identifier of the sampler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteSampler(long samplerId) throws ServerException;

	/**
	 * Creates or updates a sampler
	 * 
	 * @param sampler the sampler to save
	 * 
	 * @return the saved sampler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUISampler saveSampler(GUISampler sampler) throws ServerException;

	/**
	 * Retrieves a sampler from the data layer
	 * 
	 * @param samplerId identifier of the sampler
	 * 
	 * @return the sampler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUISampler getSampler(long samplerId) throws ServerException;

	public static class Instance {
		private static AIServiceAsync inst;

		private Instance() {
		}

		public static AIServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(AIService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}