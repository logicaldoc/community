package com.logicaldoc.gui.frontend.client.search;

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.ContextMenu;
import com.logicaldoc.gui.frontend.client.document.grid.Cursor;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsTileGrid;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel shows a list of search results in a tabular way.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class HitsListPanel extends VLayout implements SearchObserver, DocumentObserver, FolderObserver {

	private static final String FOLDER = "folder";

	protected DocumentsGrid grid = null;

	private Cursor searchCursor;

	private int visualizationMode = DocumentsGrid.MODE_LIST;

	public HitsListPanel() {
		try {
			if (CookiesManager.get(CookiesManager.COOKIE_HITSLIST_MODE) != null) {
				visualizationMode = Integer
						.parseInt(CookiesManager.get(CookiesManager.COOKIE_HITSLIST_MODE).toString());
			}
		} catch (Throwable t) {
			// Nothing to do
		}

		Search.get().addObserver(this);
		DocumentController.get().addObserver(this);
		FolderController.get().addObserver(this);
		initialize();
	}

	void initialize() {
		if (grid != null)
			removeMember((Canvas) grid);

		if (visualizationMode == DocumentsGrid.MODE_GALLERY)
			grid = new DocumentsTileGrid(null);
		else
			grid = new SearchHitsGrid();

		grid.registerSelectionChangedHandler(event -> onHitSelected());

		setContextClickHandler();

		setDoubleClickHandler();

		prepareSearchCursor();

		setMembers(searchCursor, (Canvas) grid);

		onSearchArrived();
	}

	private void setDoubleClickHandler() {
		grid.registerDoubleClickHandler(event -> {
			if (Search.get().getOptions().getType() != GUISearchOptions.TYPE_FOLDERS) {
				final GUIDocument doc = grid.getSelectedDocument();

				FolderService.Instance.get().getFolder(doc.getFolder().getId(), false, false, false,
						new AsyncCallback<GUIFolder>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIFolder folder) {
								if (folder.isDownload()
										&& "download".equals(Session.get().getInfo().getConfig("gui.doubleclick")))
									DocUtil.download(doc.getId(), null);
								else {
									PreviewPopup iv = new PreviewPopup(doc);
									iv.show();
								}
							}
						});
			}
			event.cancel();
		});
	}

	private void setContextClickHandler() {
		grid.registerCellContextClickHandler(event -> {
			GUIDocument doc = grid.getSelectedDocument();
			final String type = doc.getType();
			long id = doc.getFolder().getId();

			if (Session.get().isAdmin() || grid.getSelectedCount() == 1) {
				/*
				 * Context menu cannot be displayed for normal users in case of
				 * multiple selection
				 */
				if (type == null || (!type.contains(FOLDER) && DocumentController.get().getCurrentDocument() != null
						&& DocumentController.get().getCurrentDocument().getId() == id)) {
					showContextMenu(DocumentController.get().getCurrentDocument().getFolder(), true);
				} else {
					/*
					 * We need to retrieve the folder from the server
					 */
					FolderService.Instance.get().getFolder(id, false, false, false, new AsyncCallback<GUIFolder>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIFolder folder) {
							showContextMenu(folder, !type.contains(FOLDER));
						}
					});
				}
			}

			event.cancel();
		});
	}

	private void prepareSearchCursor() {
		if (searchCursor != null)
			removeMember(searchCursor);

		searchCursor = new Cursor();
		Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(Session.get().getUser().getHitsGrid());
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("search.hits");
		searchCursor.setPageSize(pageSize);
		searchCursor.registerPageSizeChangedHandler(event -> {
			GUISearchOptions opt = Search.get().getOptions();
			opt.setMaxHits(searchCursor.getPageSize());
			Search.get().setMaxHits(searchCursor.getPageSize());
			Search.get().search();
		});
	}

	@Override
	public void onSearchArrived() {
		/**
		 * Update the cursor. Prepare a stack for 2 sections the Title with
		 * search time and the list of hits
		 */
		NumberFormat format = NumberFormat.getFormat("#.###");

		String stats = I18N.message("aboutresults", new String[] { "" + Search.get().getEstimatedHits(),
				format.format((double) Search.get().getTime() / (double) 1000) });
		stats += " (<b>" + format.format((double) Search.get().getTime() / (double) 1000) + "</b> "
				+ I18N.message("seconds").toLowerCase() + ")";
		searchCursor.setMessage(stats);

		GUISearchOptions options = Search.get().getOptions();
		if (options.getType() == GUISearchOptions.TYPE_FULLTEXT)
			grid.setCanExpandRows();
		GUIDocument[] result = Search.get().getLastResult();
		if (result != null)
			grid.setDocuments(result);

		if (Search.get().isHasMore())
			GuiLog.warn(I18N.message("possiblemorehits"), I18N.message("possiblemorehitsdetail"));

		searchCursor.setPageSize(options.getMaxHits());
	}

	@Override
	public void onOptionsChanged(GUISearchOptions newOptions) {
		// Nothing to do
	}

	private void onHitSelected() {
		// Avoid server load in case of multiple selections
		if (grid.getSelectedCount() != 1)
			return;

		final GUIDocument hit = grid.getSelectedDocument();
		if (hit != null)
			if (hit.getType().contains(FOLDER))
				SearchPanel.get().onSelectedFolderHit(hit.getId());
			else
				SearchPanel.get().onSelectedDocumentHit(hit.getId());
	}

	private void showContextMenu(GUIFolder folder, final boolean document) {
		Menu contextMenu = new Menu();

		if (document) {
			contextMenu = new ContextMenu(folder, grid);
			if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS)) {
				MenuItem openInFolder = new MenuItem();
				openInFolder.setTitle(I18N.message("openinfolder"));
				openInFolder.addClickHandler(event -> {
					GUIDocument doc = grid.getSelectedDocument();
					DocumentsPanel.get().openInFolder(doc.getFolder().getId(), doc.getId());
				});
				contextMenu.addItem(openInFolder);
			}
		} else {
			if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS)) {
				MenuItem openInFolder = new MenuItem();
				openInFolder.setTitle(I18N.message("openfolder"));
				openInFolder.addClickHandler(event -> {
					GUIDocument doc = grid.getSelectedDocument();
					DocumentsPanel.get().openInFolder(doc.getFolder().getId(), null);
				});
				contextMenu.addItem(openInFolder);
			}
		}

		contextMenu.showContextMenu();
	}

	public DocumentsGrid getList() {
		return grid;
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		GUIDocument doc = grid.getSelectedDocument();
		if (doc != null) {
			doc.setFileName(folder.getName());
			doc.getFolder().setName(folder.getName());
			doc.getFolder().setDescription(folder.getDescription());
			grid.updateDocument(doc);
		}
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentsDeleted(GUIDocument[] documents) {
		// Nothing to do
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		grid.updateDocument(document);
	}

	@Override
	public void onDocumentCheckedIn(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentCheckedOut(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentLocked(GUIDocument document) {
		onDocumentModified(document);

	}

	@Override
	public void onDocumentUnlocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void destroy() {
		DocumentController.get().removeObserver(this);
		FolderController.get().removeObserver(this);
		Search.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	public int getVisualizationMode() {
		return visualizationMode;
	}

	public void setVisualizationMode(int visualizationMode) {
		this.visualizationMode = visualizationMode;
	}

	public DocumentsGrid getGrid() {
		return grid;
	}

	public String getGridLayout() {
		if (!(grid instanceof DocumentsListGrid))
			return null;

		DocumentsListGrid grd = (DocumentsListGrid) grid;
		if (searchCursor != null)
			return "|" + searchCursor.getPageSize() + "|" + grd.getGridLayout();
		else
			return grd.getGridLayout();
	}

	public Cursor getSearchCursor() {
		return searchCursor;
	}
}