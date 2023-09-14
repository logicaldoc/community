package com.logicaldoc.gui.frontend.client.dashboard.reading;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.frontend.client.document.DocumentsPreviewPanel;
import com.smartgwt.client.util.Offline;

/**
 * Shows a preview panels in the Reading Requests dashboard
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class ReadingRequetPreviewPanel extends DocumentsPreviewPanel {

	public ReadingRequetPreviewPanel() {
		super(CookiesManager.COOKIE_READINGS_PREV_W);
	}

	@Override
	protected void setInitialSize() {
		try {
			// Retrieve the saved preview width
			String w = (String) Offline.get(widthCookieName);
			if (w == null || w.isEmpty())
				w = "xxxx";
			setWidth(Integer.parseInt(w));
		} catch (Exception t) {
			setWidth(350);
		}
	}
}