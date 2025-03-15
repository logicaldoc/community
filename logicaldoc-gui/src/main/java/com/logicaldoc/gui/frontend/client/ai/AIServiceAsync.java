package com.logicaldoc.gui.frontend.client.ai;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.frontend.client.ai.sampler.GUISampler;

public interface AIServiceAsync {

	void deleteSampler(long samplerId, AsyncCallback<Void> callback);

	void saveSampler(GUISampler sampler, AsyncCallback<GUISampler> callback);

	void getSampler(long samplerId, AsyncCallback<GUISampler> callback);
}