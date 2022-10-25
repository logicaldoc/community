package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.frontend.client.panels.HistoryPanel;
import com.smartgwt.client.data.DataSource;

/**
 * This panel shows the history of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DcoumentHistoryPanel extends DocumentDetailTab {

	public DcoumentHistoryPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		HistoryPanel historyPanel = new HistoryPanel(true) {

			@Override
			protected DataSource getDataSource(Integer maxItems) {
				return new DocumentHistoryDS(document.getId(), maxItems);
			}
		};
		addMember(historyPanel);
	}
}