package com.logicaldoc.gui.frontend.client.ai;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.frontend.client.ai.model.GUIModel;
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
	 * Deletes some samplers
	 * 
	 * @param samplerIds identifiers of the samplers
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteSamplers(List<Long> samplerIds) throws ServerException;

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

	/**
	 * Deletes a a set of models
	 * 
	 * @param modelIds identifiers of the models to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteModels(List<Long> modelIds) throws ServerException;

	/**
	 * Creates or updates a model
	 * 
	 * @param model the model to save
	 * 
	 * @return the saved model
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIModel saveModel(GUIModel model) throws ServerException;

	/**
	 * Retrieves a model from the data layer
	 * 
	 * @param modelId identifier of the model
	 * 
	 * @return the model
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIModel getModel(long modelId) throws ServerException;

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