package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.grid.ContextMenu;
import com.logicaldoc.gui.frontend.client.document.grid.Cursor;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsTileGrid;
import com.logicaldoc.gui.frontend.client.document.grid.NavigatorDocumentsGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;

/**
 * This panel shows a selection of documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsListPanel extends VLayout {

	private DocumentsGrid grid;

	private Cursor cursor;

	private boolean filters;

	protected int visualizationMode = DocumentsGrid.MODE_LIST;

	public int getVisualizationMode() {
		return visualizationMode;
	}

	public DocumentsListPanel(GUIFolder folder, int visualizationMode) {
		this.visualizationMode = visualizationMode;

		addCursor(folder);

		if (visualizationMode == DocumentsGrid.MODE_LIST)
			grid = new NavigatorDocumentsGrid(folder);
		else if (visualizationMode == DocumentsGrid.MODE_GALLERY) {
			grid = new DocumentsTileGrid(folder);
		}
		addMember((Canvas) grid);
		grid.setGridCursor(cursor);

		registerGridHandlers();
	}

	private void registerGridHandlers() {
		grid.registerDoubleClickHandler(event -> {
			GUIDocument doc = grid.getSelectedDocument();
			long id = doc.getId();

			if (FolderController.get().getCurrentFolder().isDownload()
					&& "download".equals(Session.get().getInfo().getConfig("gui.doubleclick")))
				try {
					DocUtil.download(id, null);
				} catch (Exception t) {
					// Nothing to do
				}
			else {
				if (doc.getDocRef() != null) {
					/*
					 * re the co * in case of alias the data servlet inverts the
					 * docId and the docRef so in order to have the preview to
					 * do the right security checks we have to restorrect ids
					 */
					long aliasId = doc.getDocRef();
					doc.setDocRef(doc.getId());
					doc.setId(aliasId);
				}
				PreviewPopup iv = new PreviewPopup(doc);
				iv.show();
			}
			event.cancel();
		});

		grid.registerSelectionChangedHandler(event -> {
			// Avoid server load in case of multiple selections
			if (grid.getSelectedCount() != 1)
				return;
			GUIDocument selectedDocument = grid.getSelectedDocument();
			DocumentService.Instance.get().getById(selectedDocument.getId(), new AsyncCallback<GUIDocument>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIDocument doc) {
					DocumentController.get().setCurrentDocument(doc);
				}
			});
		});

		grid.registerCellContextClickHandler(event -> {
			Menu contextMenu = new ContextMenu(FolderController.get().getCurrentFolder(), grid);
			contextMenu.showContextMenu();
			if (event != null)
				event.cancel();
		});
	}

	private void addCursor(GUIFolder folder) {
		// Prepare a panel containing a title and the documents list
		cursor = new Cursor(true, false);
		cursor.setTotalRecords((int) folder.getDocumentCount());
		cursor.registerPageSizeChangedHandler(event -> DocumentsPanel.get().changePageSize());
		cursor.registerPageChangedHandler(event -> DocumentsPanel.get().changePageSize());
		addMember(cursor);
	}

	/**
	 * Refreshes the grid getting the documents from the given folder
	 * 
	 * @param folder the folder being opened
	 */
	public void updateData(GUIFolder folder) {
		if (grid.getFolder() == null || (grid.getFolder() != null && grid.getFolder().getId() != folder.getId()))
			grid.loadGridLayout(folder);
		DocumentsDS dataSource = new DocumentsDS(folder, null, grid.getGridCursor().getPageSize(),
				grid.getGridCursor().getCurrentPage(), null, false, false,
				(grid instanceof DocumentsListGrid ? DocumentGridUtil.getSortSpec((DocumentsListGrid) grid) : null));
		grid.fetchNewData(dataSource);

	}

	@Override
	public void destroy() {
		if (grid != null)
			grid.destroy();
	}

	public DocumentsGrid getGrid() {
		return grid;
	}

	public void toggleFilters() {
		grid.showFilters(!filters);
		filters = !filters;
	}
}