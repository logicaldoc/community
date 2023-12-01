package com.logicaldoc.gui.frontend.client.document.selector;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.frontend.client.document.DocumentsListPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;

/**
 * A documents panel for the selection dialog
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class DocumentSelectorDocumentsPanel extends DocumentsListPanel {

	public DocumentSelectorDocumentsPanel(GUIFolder folder) {
		super(folder, DocumentsGrid.MODE_LIST);
		setCanDrag(false);
	}

	@Override
	protected void registerSelectionHandler() {
		// Do nothing
	}

	@Override
	protected DocumentsGrid prepareDocumentsGrid(GUIFolder folder, int visualizationMode) {
		return new DocumentSelectorDocumentsListGrid(folder);
	}
}