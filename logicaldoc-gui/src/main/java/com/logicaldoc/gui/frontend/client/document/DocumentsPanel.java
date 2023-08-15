package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.grid.NavigatorDocumentsGrid;
import com.logicaldoc.gui.frontend.client.folder.FolderDetailsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel implements the browser on the documents archive
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsPanel extends HLayout implements FolderObserver, DocumentObserver {

	protected Layout listing = new VLayout();

	protected Layout details = new VLayout();

	protected Canvas listingPanel;

	protected Canvas detailPanel;

	protected static DocumentsPanel instance;

	protected VLayout body = new VLayout();

	protected GUIFolder folder;

	protected DocumentsMenu documentsMenu;

	protected DocumentsPreviewPanel previewPanel;

	protected int visualizationMode = DocumentsGrid.MODE_LIST;

	protected boolean initialized = false;

	private DocumentsPanel() {
		setWidth100();
		setOverflow(Overflow.HIDDEN);
		setShowEdges(false);
	}

	public DocumentsMenu getDocumentsMenu() {
		return documentsMenu;
	}

	public static DocumentsPanel get() {
		if (instance == null) {
			instance = new DocumentsPanel();
			DocumentController.get().addObserver(instance);

			int mode = DocumentsGrid.MODE_LIST;
			if (CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE) != null
					&& !CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE).equals(""))
				mode = Integer.parseInt(CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MODE));
			instance.setMode(mode);
		}
		return instance;
	}

	@Override
	public void onDraw() {
		if (initialized)
			return;

		// Register to folders events
		FolderController.get().addObserver(this);

		// Register to documents events
		DocumentController.get().addObserver(this);

		// Initialize the listing panel as placeholder
		listingPanel = new Label("&nbsp;" + I18N.message("selectfolder"));
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("51%");
		listing.setShowResizeBar(true);
		listing.addMember(listingPanel);

		// Add a details panel under the listing one
		detailPanel = new Label("&nbsp;" + I18N.message("selectfolderordoc"));
		details.setAlign(Alignment.CENTER);
		details.addMember(detailPanel);

		// The two rows: listing and details
		VLayout listingAndDetails = new VLayout();
		listingAndDetails.setMembers(listing, details);
		listingAndDetails.setShowResizeBar(true);
		listingAndDetails.setResizeBarTarget("next");

		previewPanel = new DocumentsPreviewPanel();

		// The listing plus the preview
		HLayout bodyPanel = new HLayout();
		bodyPanel.setWidth100();
		bodyPanel.setMembers(listingAndDetails, previewPanel);

		body.setMembers(DocumentToolbar.get(), bodyPanel);

		documentsMenu = new DocumentsMenu();

		setMembers(documentsMenu, body);

		previewPanel.addVisibilityChangedHandler(event -> {
			if (detailPanel instanceof DocumentDetailsPanel)
				previewPanel.setDocument(((DocumentDetailsPanel) detailPanel).getDocument());
		});

		initialized = true;
	}

	public void openInFolder(long folderId, Long docId) {
		if (!initialized)
			onDraw();

		MainPanel.get().selectDocumentsTab();
		FolderNavigator.get().openFolder(folderId, docId);
		documentsMenu.expandSection(0);
		if (detailPanel instanceof DocumentDetailsPanel)
			((DocumentDetailsPanel) detailPanel).selectDefaultTab();
	}

	public void openInFolder(long docId) {
		DocumentService.Instance.get().getById(docId, new AsyncCallback<GUIDocument>() {
			@Override
			public void onFailure(Throwable caught) {
				/*
				 * Sometimes we can have spurious errors using Firefox.
				 */
				if (Session.get().isDevel())
					GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument result) {
				DocumentController.get().selected(result);
				GUIFolder fld = result.getFolder();
				if (fld != null) {
					openInFolder(fld.getId(), result.getId());
				}
			}
		});
	}

	/**
	 * Shows the document details of a selected document. The documents details
	 * are retrieved from the server.
	 * 
	 * @param docId Id of the documents that needs to be selected
	 * @param clearSelection true if you want to de-select all records in the
	 *        list
	 */
	public void selectDocument(long docId, final boolean clearSelection) {
		DocumentService.Instance.get().getById(docId, new AsyncCallback<GUIDocument>() {
			@Override
			public void onFailure(Throwable caught) {
				/*
				 * Sometimes we can have spurious errors using Firefox.
				 */
				if (Session.get().isDevel())
					GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument result) {
				if (!(detailPanel instanceof DocumentDetailsPanel)) {
					details.removeMember(detailPanel);
					detailPanel.destroy();
					detailPanel = new DocumentDetailsPanel();
					details.addMember(detailPanel);
				}

				DocumentToolbar.get().update(result, null);
				if (detailPanel instanceof DocumentDetailsPanel) {
					((DocumentDetailsPanel) detailPanel).setDocument(result);
					details.redraw();
				}

				if (clearSelection)
					((DocumentsListPanel) listingPanel).getGrid().deselectAll();
			}
		});
	}

	public void refresh() {
		refresh((Integer) null);
	}

	public void refresh(Integer visualizationMode) {
		if (visualizationMode != null)
			this.visualizationMode = visualizationMode;
		updateListingPanel(folder);

		// Avoid to show the folder's details panel in case of open in folder
		// where a doc ID has been specified
		if (Session.get().getHiliteDocId() == null)
			scheduleFolderDetailsRefresh();
	}

	public void changePageSize() {
		if (listingPanel instanceof DocumentsListPanel
				&& ((DocumentsListPanel) listingPanel).getVisualizationMode() == visualizationMode) {
			((DocumentsListPanel) listingPanel).updateData(folder);
		} else {
			if (listingPanel != null) {
				listing.removeMember(listingPanel);
				listingPanel.destroy();
			}
			listingPanel = new DocumentsListPanel(folder, visualizationMode);
			listing.addMember(listingPanel);
			listing.redraw();
		}
		previewPanel.reset();

		scheduleFolderDetailsRefresh();
	}

	/*
	 * Launches the refresh of the details panel in another thread because the
	 * synchronous invocation makes the observers notification's thread to stop.
	 */
	private void scheduleFolderDetailsRefresh() {
		Timer timer = new Timer() {
			public void run() {
				showFolderDetails();
			}
		};
		timer.schedule(100);
	}

	private void updateListingPanel(GUIFolder folder) {
		if (listingPanel instanceof DocumentsListPanel
				&& ((DocumentsListPanel) listingPanel).getVisualizationMode() == visualizationMode) {
			((DocumentsListPanel) listingPanel).updateData(folder);
		} else {
			if (listingPanel != null) {
				listing.removeMember(listingPanel);
				listingPanel.destroy();
			}
			listingPanel = new DocumentsListPanel(folder, visualizationMode);
			listing.addMember(listingPanel);
			listing.redraw();
		}
		previewPanel.reset();
	}

	/**
	 * Shows folders data in the details area.
	 */
	public void showFolderDetails() {
		if (detailPanel != null) {
			details.removeMember(detailPanel);
			detailPanel.destroy();
		}
		detailPanel = new FolderDetailsPanel(folder);
		details.addMember(detailPanel);
		details.redraw();
	}

	public void toggleFilters() {
		if (listingPanel instanceof DocumentsListPanel) {
			((DocumentsListPanel) listingPanel).toggleFilters();
		}
	}

	public void printPreview() {
		if (listingPanel instanceof DocumentsListPanel) {
			GridUtil.print((DocumentsListGrid) ((DocumentsListPanel) listingPanel).getGrid());
		}
	}

	public void export() {
		if (listingPanel instanceof DocumentsListPanel
				&& ((DocumentsListPanel) listingPanel).getGrid() instanceof DocumentsListGrid)
			GridUtil.exportCSV((DocumentsListGrid) ((DocumentsListPanel) listingPanel).getGrid(), false);
	}

	public GUIDocument getSelectedDocument() {
		if (listingPanel instanceof DocumentsListPanel)
			return ((DocumentsListPanel) listingPanel).getGrid().getSelectedDocument();
		else
			return null;
	}

	public DocumentsGrid getDocumentsGrid() {
		return ((DocumentsListPanel) listingPanel).getGrid();
	}

	public int getMode() {
		return visualizationMode;
	}

	public void setMode(int mode) {
		this.visualizationMode = mode;
	}

	public DocumentsPreviewPanel getPreviewPanel() {
		return previewPanel;
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		if (DocumentController.get().getCurrentDocument() != null
				&& DocumentController.get().getCurrentDocument().getId() == document.getId()
				&& !DocumentController.get().isEditing(document)) {
			previewPanel.setDocument(document);

			if (DocumentController.get().isEditing(document))
				onDocumentCancelEditing(document);
		}
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		DocumentsGrid grid = getDocumentsGrid();
		if (grid.getSelectedDocument().getId() == document.getId()) {
			disableAll();
		}
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		DocumentsGrid grid = getDocumentsGrid();
		if (grid.getSelectedDocument().getId() == document.getId()
				|| (document.getDocRef() != null && grid.getSelectedDocument().getId() == document.getDocRef()))
			enableAll();
	}

	private void enableAll() {
		listingPanel.enable();
		DocumentToolbar.get().enable();
		documentsMenu.enable();
	}

	private void disableAll() {
		listingPanel.disable();
		DocumentToolbar.get().disable();
		documentsMenu.disable();
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		if (!MainPanel.get().isOnDocumentsTab())
			return;

		if ((folder.getFoldRef() != null && folder.getFoldRef() != document.getFolder().getId())
				|| (folder.getFoldRef() == null && folder.getId() != document.getFolder().getId()))
			return;

		if (detailPanel != null && !(detailPanel instanceof DocumentDetailsPanel)) {
			details.removeMember(detailPanel);
			detailPanel.destroy();
			detailPanel = new DocumentDetailsPanel();
			details.addMember(detailPanel);
		}

		DocumentToolbar.get().update(document, null);
		if (detailPanel instanceof DocumentDetailsPanel) {
			((DocumentDetailsPanel) detailPanel).setDocument(document);
			details.redraw();
		}

		previewPanel.setDocument(document);
	}

	@Override
	public void onDocumentsDeleted(GUIDocument[] documents) {
		for (GUIDocument doc : documents)
			if (DocumentsPanel.get().getSelectedDocument() != null
					&& doc.getId() == DocumentsPanel.get().getSelectedDocument().getId()) {
				DocumentsPanel.get().showFolderDetails();
				previewPanel.reset();
				break;
			}

		DocumentsPanel.get().getDocumentsMenu().refresh("trash");
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		if (folder != null && document.getFolder().getId() != folder.getId())
			onDocumentsDeleted(new GUIDocument[] { document });
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
	public void onFolderDeleted(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		if (FolderController.get().getCurrentFolder().getId() == folder.getId()) {
			enableAll();
			if (listingPanel != null) {
				listing.removeMember(listingPanel);
				listingPanel.destroy();
				listingPanel = null;
			}
			onFolderSelected(folder);
		}
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
	public void onFolderSelected(GUIFolder folder) {
		this.folder = folder;
		// Reset the cursor to the first page
		if (listingPanel instanceof DocumentsListPanel)
			((DocumentsListPanel) listingPanel).getGrid().getGridCursor().setCurrentPage(1);
		refresh();
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		if (FolderController.get().getCurrentFolder().getId() == folder.getId())
			disableAll();
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		if (detailPanel instanceof FolderDetailsPanel) {
			FolderDetailsPanel fPanel = (FolderDetailsPanel) detailPanel;
			if (fPanel.getFolder().getId() == folder.getId())
				enableAll();
		}
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
		DocumentController.get().removeObserver(this);
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

	public String getDocsGridViewState() {
		if (listingPanel instanceof DocumentsListPanel) {
			DocumentsListPanel docsListingPanel = (DocumentsListPanel) listingPanel;
			if (docsListingPanel.getGrid() instanceof NavigatorDocumentsGrid)
				return ((NavigatorDocumentsGrid) docsListingPanel.getGrid()).getGridLayout();
			else
				return null;
		} else
			return null;
	}
}