package com.logicaldoc.gui.frontend.client.google;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.OAuthException;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;

/**
 * Useful to handle communication with Google service
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class GoogleAsyncCallback<T> extends DefaultAsyncCallback<T> {
	@Override
	public void onFailure(Throwable caught) {
		LD.clearPrompt();
		if (caught instanceof OAuthException)
			GoogleApiAuthorization.get().show();
		else
			GuiLog.serverError(caught);
	}
}