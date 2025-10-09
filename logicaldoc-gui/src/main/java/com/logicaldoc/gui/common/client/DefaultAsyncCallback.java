package com.logicaldoc.gui.common.client;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;

/**
 * An implemantation of callback that logs the server errors and clears the
 * prompt
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 *
 * @param <T> The type of the return value that was declared in the synchronous
 *        version of the service method.
 */
public abstract class DefaultAsyncCallback<T> implements AsyncCallback<T> {

	@Override
	public void onFailure(Throwable caught) {
		LD.clearPrompt();
		GuiLog.serverError(caught);
	}

	@Override
	public void onSuccess(T v) {
		LD.clearPrompt();
		handleSuccess(v);
	}
	
	protected abstract void handleSuccess(T result);
}