package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface AnnotationsServiceAsync {

	void prepareAnnotations(long docId, String fileVersion, AsyncCallback<Integer> callback);

	void addAnnotation(long docId, int page, String snippet, String text, AsyncCallback<Long> callback);

	void savePage(long docId, int page, String content, AsyncCallback<Void> callback);

}
