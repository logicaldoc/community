package com.logicaldoc.gui.common.client;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;

/**
 * An abstract implemantation of callback that logs the server errors
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 *
 * @param <T>
 */
public abstract class GUIAsyncCallback<T> implements AsyncCallback<T> {

	@Override
	public void onFailure(Throwable caught) {
		LD.clearPrompt();
		GuiLog.serverError(caught);
	}
}