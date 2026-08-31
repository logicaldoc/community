package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.frontend.client.document.DocumentsPreviewPanel;

/**
 * Shows a preview panels in the Search workspace
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class SearchPreviewPanel extends DocumentsPreviewPanel {

	public SearchPreviewPanel() {
		super(CookiesManager.COOKIE_HITSLIST_PREV_W);
	}
}