package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.FolderHistoryDS;
import com.logicaldoc.gui.frontend.client.panels.HistoryPanel;
import com.smartgwt.client.data.DataSource;

/**
 * This panel shows the history of a folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderHistoryPanel extends FolderDetailTab {

	public FolderHistoryPanel(final GUIFolder folder) {
		super(folder, null);
	}

	@Override
	protected void onDraw() {
		HistoryPanel historyPanel = new HistoryPanel(false) {

			@Override
			protected DataSource getDataSource(Integer maxItems) {
				return new FolderHistoryDS(folder.getId(), maxItems);
			}
		};
		addMember(historyPanel);
	}
}