package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.grid.ContextMenu;
import com.logicaldoc.gui.frontend.client.document.grid.Cursor;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsTileGrid;
import com.logicaldoc.gui.frontend.client.document.grid.GridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.NavigatorDocumentsGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
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

	public DocumentsListPanel(GUIFolder folder, Integer max, int visualizationMode) {
		this.visualizationMode = visualizationMode;

		if (visualizationMode == DocumentsGrid.MODE_LIST)
			grid = new NavigatorDocumentsGrid(folder, max);
		else if (visualizationMode == DocumentsGrid.MODE_GALLERY) {
			DocumentsDS dataSource = new DocumentsDS(folder, null, max, 1, null, false, false, null);
			grid = new DocumentsTileGrid(folder, dataSource);
		}

		// Prepare a panel containing a title and the documents list
		cursor = new Cursor(true, false);
		cursor.setTotalRecords(folder.getDocumentCount());

		cursor.registerPageSizeChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				DocumentsPanel.get().changePageSize(cursor.getPageSize());
			}
		});
		cursor.registerPageChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				DocumentsPanel.get().changePageSize(cursor.getPageSize());
			}
		});

		addMember(cursor);
		addMember((Canvas) grid);

		grid.setGridCursor(cursor);

		grid.registerDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				GUIDocument doc = grid.getSelectedDocument();
				long id = doc.getId();

				if (Session.get().getCurrentFolder().isDownload()
						&& "download".equals(Session.get().getInfo().getConfig("gui.doubleclick")))
					try {
						WindowUtils.openUrl(Util.downloadURL(id));
					} catch (Throwable t) {

					}
				else {
					if (doc.getDocRef() != null) {
						/*
						 * in case of alias the data servlet inverts the docId
						 * and the docRef so in order to have the preview to do
						 * the right security checks we have to restore the
						 * correct ids
						 */
						long aliasId = doc.getDocRef();
						doc.setDocRef(doc.getId());
						doc.setId(aliasId);
					}
					PreviewPopup iv = new PreviewPopup(doc);
					iv.show();
				}

				event.cancel();
			}
		});

		grid.registerSelectionChangedHandler(new SelectionChangedHandler() {
			@Override
			public void onSelectionChanged(SelectionEvent event) {
				// Avoid server load in case of multiple selections
				if (grid.getSelectedCount() != 1)
					return;
				DocumentService.Instance.get().getById(grid.getSelectedDocument().getId(),
						new AsyncCallback<GUIDocument>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument doc) {
								Session.get().setCurrentDocument(doc);
							}
						});
			}
		});

		grid.registerCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = new ContextMenu(Session.get().getCurrentFolder(), grid);
				contextMenu.showContextMenu();
				if (event != null)
					event.cancel();
			}
		});
	}

	/**
	 * Refreshes the grid getting the documents from the given folder
	 * 
	 * @param folder the folder being opened
	 * @param max maximum number of elements
	 */
	public void updateData(GUIFolder folder, Integer max) {
		if (grid.getFolder() == null || (grid.getFolder() != null && grid.getFolder().getId() != folder.getId()))
			grid.loadGridLayout(folder);
		DocumentsDS dataSource = new DocumentsDS(folder, null, max, grid.getGridCursor().getCurrentPage(), null, false,
				false, (grid instanceof DocumentsListGrid ? GridUtil.getSortSpec((DocumentsListGrid) grid) : null));
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