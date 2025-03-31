package com.logicaldoc.gui.frontend.client.ai;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.frontend.client.ai.model.GUIModel;
import com.logicaldoc.gui.frontend.client.ai.model.GUIPredictionResult;
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

	void predict(long modelId, List<String> features, AsyncCallback<List<GUIPredictionResult>> callback)
			throws ServerException;
}