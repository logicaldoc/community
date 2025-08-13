package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.data.DocumentsDSParameters;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
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

/**
 * This panel shows a selection of documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsListPanel extends VLayout {

	protected DocumentsGrid documentsGrid;

	protected Cursor cursor;

	private boolean filters;

	protected int visualizationMode = DocumentsGrid.MODE_LIST;

	public DocumentsListPanel(GUIFolder folder, int visualizationMode) {
		this.visualizationMode = visualizationMode;

		this.cursor = prepareCursor(folder);
		addMember(cursor);

		documentsGrid = prepareDocumentsGrid(folder, visualizationMode);

		addMember((Canvas) documentsGrid);
		documentsGrid.setGridCursor(cursor);

		registerGridHandlers();
	}

	protected DocumentsGrid prepareDocumentsGrid(GUIFolder folder, int visualizationMode) {
		DocumentsGrid docsGrid = null;
		if (visualizationMode == DocumentsGrid.MODE_LIST)
			docsGrid = new NavigatorDocumentsGrid(folder);
		else if (visualizationMode == DocumentsGrid.MODE_GALLERY) {
			docsGrid = new DocumentsTileGrid(folder);
		}
		return docsGrid;
	}

	public int getVisualizationMode() {
		return visualizationMode;
	}

	private void registerGridHandlers() {
		documentsGrid.registerDoubleClickHandler(event -> {
			GUIDocument doc = documentsGrid.getSelectedDocument();
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
				new PreviewPopup(doc).show();
			}
			event.cancel();
		});

		registerSelectionHandler();

		documentsGrid.registerCellContextClickHandler(click -> {
			DocumentService.Instance.get().getAllowedPermissions(documentsGrid.getSelectedIds(),
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(GUIAccessControlEntry enabledPermissions) {
							new ContextMenu(FolderController.get().getCurrentFolder(), documentsGrid,
									enabledPermissions).showContextMenu();
						}
					});
			if (click != null)
				click.cancel();
		});
	}

	protected void registerSelectionHandler() {
		documentsGrid.registerSelectionChangedHandler(event -> {
			// Avoid server load in case of multiple selections
			if (documentsGrid.getSelectedCount() != 1)
				return;
			GUIDocument selectedDocument = documentsGrid.getSelectedDocument();
			DocumentService.Instance.get().getById(selectedDocument.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument doc) {
					DocumentController.get().setCurrentDocument(doc);
				}
			});
		});
	}

	protected Cursor prepareCursor(GUIFolder folder) {
		// Prepare a panel containing a title and the documents list
		Cursor crsr = new Cursor(true, false);
		crsr.setTotalRecords((int) folder.getDocumentCount());
		crsr.registerPageSizeChangedHandler(event -> DocumentsPanel.get().changePageSize());
		crsr.registerPageChangedHandler(event -> DocumentsPanel.get().changePageSize());
		return crsr;
	}

	/**
	 * Refreshes the grid getting the documents from the given folder
	 * 
	 * @param folder the folder being opened
	 */
	public void updateData(GUIFolder folder) {
		if (documentsGrid.getFolder() == null
				|| (documentsGrid.getFolder() != null && documentsGrid.getFolder().getId() != folder.getId()))
			documentsGrid.loadGridLayout(folder);

		DocumentsDSParameters params = new DocumentsDSParameters(folder.getId(), null,
				documentsGrid.getGridCursor().getPageSize(), documentsGrid.getGridCursor().getCurrentPage(),
				documentsGrid instanceof DocumentsListGrid listGrid ? DocumentGridUtil.getSortSpec(listGrid) : null);
		DocumentsDS dataSource = new DocumentsDS(params);
		documentsGrid.fetchNewData(dataSource);
		documentsGrid.setCanDrag(folder.isMove());
	}

	@Override
	public void destroy() {
		if (documentsGrid != null)
			documentsGrid.destroy();
	}

	public DocumentsGrid getGrid() {
		return documentsGrid;
	}

	public void toggleFilters() {
		documentsGrid.showFilters(!filters);
		filters = !filters;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}