package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
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
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsTileGrid;
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

		DocumentsDS dataSource = new DocumentsDS(folder, null, max, 1, null, false, false, true);
		if (visualizationMode == DocumentsGrid.MODE_LIST)
			grid = new NavigatorDocumentsGrid(dataSource, folder);
		else if (visualizationMode == DocumentsGrid.MODE_GALLERY)
			grid = new DocumentsTileGrid(folder, dataSource);

		// Prepare a panel containing a title and the documents list
		cursor = new Cursor(CookiesManager.COOKIE_DOCSLIST_MAX, true, false);
		cursor.setTotalRecords(folder.getDocumentCount());

		cursor.registerMaxChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				DocumentsPanel.get().refresh(cursor.getMaxDisplayedRecords(), null);
			}
		});
		cursor.registerPageChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				DocumentsPanel.get().refresh(cursor.getMaxDisplayedRecords(), null);
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

	public void updateData(GUIFolder folder, Integer max) {
		DocumentsDS dataSource = new DocumentsDS(folder, null, max, grid.getGridCursor().getCurrentPage(), null, false,
				false, true);
		grid.fetchNewData(dataSource);
		grid.loadGridLayout(folder);
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