package com.logicaldoc.gui.frontend.client.google;

import com.logicaldoc.gui.common.client.OAuthException;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;

/**
 * Some utility methods for Gootle
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 *
 */
public class GoogleUtil {

	private GoogleUtil() {
		// empty
	}

	public static void handleGoogleServiceError(Throwable caught) {
		LD.clearPrompt();
		if(caught instanceof OAuthException)
			GoogleApiAuthorization.get().show();
		else
			GuiLog.serverError(caught);
	}
}
