package com.logicaldoc.gui.common.client;

import com.google.gwt.user.client.rpc.AsyncCallback;

/**
 * An implemantation of callback that ignores both successes and failures
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 *
 * @param <T>
 */
public class IgnoreAsyncCallback<T> implements AsyncCallback<T> {

	@Override
	public void onFailure(Throwable caught) {
		// Do nothing
	}

	@Override
	public void onSuccess(T v) {
		// Do nothing
	}
}