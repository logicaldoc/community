package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.grid.NavigatorDocumentsGrid;
import com.logicaldoc.gui.frontend.client.folder.FolderDetailsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.Offline;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.VisibilityChangedEvent;
import com.smartgwt.client.widgets.events.VisibilityChangedHandler;
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

	protected Integer max;

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

			// Setup the default elements number
			String mx = "100";
			if (CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MAX) != null
					&& !Offline.get(CookiesManager.COOKIE_DOCSLIST_MAX).equals(""))
				mx = CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_MAX);
			instance.setMax(Integer.parseInt(mx));

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

		previewPanel.addVisibilityChangedHandler(new VisibilityChangedHandler() {

			@Override
			public void onVisibilityChanged(VisibilityChangedEvent event) {
				if (detailPanel instanceof DocumentDetailsPanel)
					previewPanel.setDocument(((DocumentDetailsPanel) detailPanel).getDocument());
			}
		});

		initialized = true;
	}

	public void openInFolder(long folderId, Long docId) {
		if (!initialized)
			onDraw();

		MainPanel.get().selectDocumentsTab();
		FolderNavigator.get().openFolder(folderId, docId);
		documentsMenu.expandSection(0);
		if (detailPanel != null && detailPanel instanceof DocumentDetailsPanel)
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
					Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument result) {
				DocumentController.get().selected(result);
				GUIFolder folder = result.getFolder();
				if (folder != null) {
					openInFolder(folder.getId(), result.getId());
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
					Log.serverError(caught);
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
		refresh(null, null);
	}

	public void refresh(Integer max, Integer visualizationMode) {
		if (max != null && max > 0)
			this.max = max;
		if (visualizationMode != null)
			this.visualizationMode = visualizationMode;

		updateListingPanel(folder);

		/*
		 * Launch the refresh of the details panel in another thread because the
		 * synchronous invocation makes the observers notification's thread to
		 * stop.
		 */
		Timer timer = new Timer() {
			public void run() {
				showFolderDetails();
			}
		};
		timer.schedule(100);
	}

	private void updateListingPanel(GUIFolder folder) {
		if (listingPanel != null && listingPanel instanceof DocumentsListPanel
				&& ((DocumentsListPanel) listingPanel).getVisualizationMode() == visualizationMode) {
			((DocumentsListPanel) listingPanel).updateData(folder, max);
		} else {
			listing.removeMember(listingPanel);
			listingPanel.destroy();
			listingPanel = new DocumentsListPanel(folder, max, visualizationMode);
			listing.addMember(listingPanel);
			listing.redraw();
		}
		previewPanel.reset();
	}

	/**
	 * Shows folders data in the details area.
	 */
	public void showFolderDetails() {
		if (detailPanel != null)
			details.removeMember(detailPanel);
		detailPanel.destroy();
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
			Canvas.printComponents(new Object[] { ((DocumentsListPanel) listingPanel).getGrid() });
		}
	}

	public void export() {
		if (listingPanel instanceof DocumentsListPanel)
			if (((DocumentsListPanel) listingPanel).getGrid() instanceof DocumentsListGrid)
				Util.exportCSV((DocumentsListGrid) ((DocumentsListPanel) listingPanel).getGrid(), false);
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

	public Integer getMax() {
		return max;
	}

	public void setMax(Integer max) {
		this.max = max;
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
		if (Session.get().getCurrentDocument() != null
				&& Session.get().getCurrentDocument().getId() == document.getId())
			previewPanel.setDocument(document);
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		if (!MainPanel.get().isOnDocumentsTab())
			return;

		if (folder.getId() != document.getFolder().getId())
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

	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
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
	public void onFolderSelected(GUIFolder folder) {
		this.folder = folder;
		// Reset the cursor to the first page
		if (listingPanel != null && listingPanel instanceof DocumentsListPanel)
			((DocumentsListPanel) listingPanel).getGrid().getGridCursor().setCurrentPage(1);
		refresh();
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
		DocumentController.get().removeObserver(this);
	}

	@Override
	protected void finalize() throws Throwable {
		destroy();
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
		if (listingPanel != null && listingPanel instanceof DocumentsListPanel) {
			DocumentsListPanel docsListingPanel = (DocumentsListPanel) listingPanel;
			if (docsListingPanel.getGrid() instanceof NavigatorDocumentsGrid)
				return ((NavigatorDocumentsGrid) docsListingPanel.getGrid()).getViewState();
			else
				return null;
		} else
			return null;
	}
}