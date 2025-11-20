package com.logicaldoc.gui.frontend.client.ai;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.frontend.client.ai.model.GUIModel;
import com.logicaldoc.gui.frontend.client.ai.model.GUIQueryResult;
import com.logicaldoc.gui.frontend.client.ai.sampler.GUISampler;

public interface AIServiceAsync {

	void deleteSamplers(List<Long> samplerIds, AsyncCallback<Void> callback);

	void saveSampler(GUISampler sampler, AsyncCallback<GUISampler> callback);

	void getSampler(long samplerId, AsyncCallback<GUISampler> callback);

	void deleteModels(List<Long> modelIds, AsyncCallback<Void> callback);

	void saveModel(GUIModel model, AsyncCallback<GUIModel> callback);

	void getModel(long modelId, AsyncCallback<GUIModel> callback);

	void getModels(AsyncCallback<List<GUIModel>> callback);

	void trainModel(long modelId, AsyncCallback<Void> callback);

	void evaluateModel(long modelId, AsyncCallback<Void> callback);

	void query(long modelId, List<String> features, AsyncCallback<List<GUIQueryResult>> callback);

	void importModel(String modelName, AsyncCallback<GUIModel> callback);

	void cloneModel(long modelId, String newName, AsyncCallback<GUIModel> callback);

	void getStats(Long modelId, Long tenantId, AsyncCallback<List<GUIParameter>> callaback);

	void loadVectorStore(AsyncCallback<List<GUIParameter>> callaback) throws ServerException;

	void saveVectorStore(List<GUIParameter> settings, AsyncCallback<Void> callback) throws ServerException;

	void testVectorStore(List<GUIParameter> settings, AsyncCallback<Boolean> callaback) throws ServerException;
}