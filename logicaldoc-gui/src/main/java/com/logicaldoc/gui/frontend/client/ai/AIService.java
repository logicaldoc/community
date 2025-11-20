package com.logicaldoc.gui.frontend.client.ai;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.frontend.client.ai.model.GUIModel;
import com.logicaldoc.gui.frontend.client.ai.model.GUIQueryResult;
import com.logicaldoc.gui.frontend.client.ai.sampler.GUISampler;

/**
 * The client side stub for the Artificial Intelligence Service. This service
 * gives all needed methods to handle AI models and samplers.
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

	/**
	 * Retrieves all the models
	 * 
	 * @return the list of models
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIModel> getModels() throws ServerException;

	/**
	 * Trains a model
	 * 
	 * @param modelId identifier of the model to train
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void trainModel(long modelId) throws ServerException;

	/**
	 * Evaluated a neural network model
	 * 
	 * @param modelId identifier of the neural network model to evaluate
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void evaluateModel(long modelId) throws ServerException;

	/**
	 * Runs a model and gets the prediction
	 * 
	 * @param modelId identifier of the model
	 * @param features ordered list of feature values
	 * 
	 * @return the list of predictions ordered by descending score 
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIQueryResult> query(long modelId, List<String> features) throws ServerException;
	
	/**
	 * Imports a new model
	 * 
	 * @param modelName Name to give to the new imported model
	 * 
	 * @return The created model
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIModel importModel(String modelName) throws ServerException;
	
	/**
	 * Clones a model
	 * 
	 * @param modelId Identifier of the model to clone
	 * @param newName The name to give to the clone
	 * 
	 * @return The clone
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIModel cloneModel(long modelId, String newName) throws ServerException;
	
	/**
	 * Loads the statistics from of the AI
	 * 
	 * @param modelId Optional indentifier of the model
	 * @param tenantId Optional indentifier of the tenant
	 * 
	 * @return all the stats
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> getStats(Long modelId, Long tenantId) throws ServerException;
	
	
	/**
	 * Loads all the settings related to the vector store
	 * 
	 * @return List of settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadVectorStore() throws ServerException;
	
	/**
	 * Saves all the settings related to the vector store
	 * 
	 * @param settings The vector store's settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveVectorStore(List<GUIParameter> settings) throws ServerException;
	
	/**
	 * Checks the connection to the vector store
	 * 
	 * @param settings The vector store's settings, they are not persisted but just used to test the connection
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean testVectorStore(List<GUIParameter> settings) throws ServerException;
	
	
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