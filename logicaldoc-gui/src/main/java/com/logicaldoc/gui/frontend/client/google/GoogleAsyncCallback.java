package com.logicaldoc.gui.frontend.client.google;

import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.OAuthException;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;

/**
 * Useful to handle communication with Google service
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 * 
 * @param <T> The type of the return value that was declared in the synchronous
 *        version of the service method.
 */
public class GoogleAsyncCallback<T> extends EmptyAsyncCallback<T> {
	@Override
	public void onFailure(Throwable caught) {
		LD.clearPrompt();
		if (caught instanceof OAuthException)
			new GoogleApiAuthorization().show();
		else
			GuiLog.serverError(caught);
	}
}