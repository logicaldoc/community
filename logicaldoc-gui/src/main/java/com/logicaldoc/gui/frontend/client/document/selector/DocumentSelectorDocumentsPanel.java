package com.logicaldoc.gui.frontend.client.document.selector;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.frontend.client.document.DocumentsListPanel;
import com.logicaldoc.gui.frontend.client.document.grid.Cursor;
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
	protected Cursor prepareCursor(GUIFolder folder) {
		// Prepare a panel containing a title and the documents list
		Cursor cursor = new Cursor(true, false);
		cursor.setTotalRecords((int) folder.getDocumentCount());
		cursor.registerPageSizeChangedHandler(event -> changePageSize());
		cursor.registerPageChangedHandler(event -> changePageSize());
		return cursor;
	}

	private void changePageSize() {
		this.updateData(documentsGrid.getFolder());
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