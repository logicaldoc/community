package com.logicaldoc.gui.frontend.client.document;

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
import com.logicaldoc.gui.frontend.client.folder.FolderDetailsPanel;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
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
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class DocumentsPanel extends HLayout implements FolderObserver, DocumentObserver {

	protected Layout listing = new VLayout();

	protected Layout details = new VLayout();

	protected Canvas listingPanel;

	protected Canvas detailPanel;

	protected static DocumentsPanel instance;

	protected DocumentToolbar toolbar;

	protected VLayout body = new VLayout();

	protected GUIFolder folder;

	protected DocumentsMenu documentsMenu;

	protected DocumentsPreviewPanel previewPanel;

	public DocumentsMenu getDocumentsMenu() {
		return documentsMenu;
	}

	// The document that must be hilighted
	protected Long hiliteDocId = null;

	protected Integer max;

	protected int mode = DocumentsGrid.MODE_LIST;

	protected DocumentsPanel() {
		// Register to folders events
		FolderController.get().addObserver(this);

		// Register to documents events
		DocumentController.get().addObserver(this);

		setWidth100();
		setOverflow(Overflow.HIDDEN);

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

		toolbar = new DocumentToolbar();
		toolbar.setWidth100();

		body.addMember(toolbar);
		body.addMember(listing);
		body.addMember(details);
		body.setShowResizeBar(true);
		body.setResizeBarTarget("next");

		documentsMenu = new DocumentsMenu();
		previewPanel = new DocumentsPreviewPanel();

		setMembers(documentsMenu, body, previewPanel);
		setShowEdges(false);

		previewPanel.addVisibilityChangedHandler(new VisibilityChangedHandler() {

			@Override
			public void onVisibilityChanged(VisibilityChangedEvent event) {
				if (detailPanel instanceof DocumentDetailsPanel)
					previewPanel.setDocument(((DocumentDetailsPanel) detailPanel).getDocument());
			}
		});
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

	public void openInFolder(long folderId, Long docId) {
		// Save the information about the document that will be hilighted by
		// habler onFolderSelect
		if (docId != null)
			hiliteDocId = docId;

		documentsMenu.openFolder(folderId);
		documentsMenu.expandSection(0);
		MainPanel.get().selectDocumentsTab();
		if (detailPanel != null && detailPanel instanceof DocumentDetailsPanel)
			((DocumentDetailsPanel) detailPanel).selectDeafultTab();
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

				toolbar.update(result);
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
		refresh(null, 1, null);
	}

	public void refresh(Integer max, int page, Integer mode) {
		if (max != null && max > 0)
			this.max = max;

		if (mode != null)
			this.mode = mode;

		updateListingPanel(folder, hiliteDocId, this.max, page, this.mode);

		showFolderDetails();

		hiliteDocId = null;
	}

	protected void updateListingPanel(GUIFolder folder, Long hiliteDocId, Integer max, int page, int mode) {
		listing.removeMember(listingPanel);
		listingPanel.destroy();
		listingPanel = new DocumentsListPanel(folder, hiliteDocId, max, page, mode);
		listing.addMember(listingPanel);
		listing.redraw();

		previewPanel.reset();
	}

	/**
	 * Shows folders data in the details area.
	 */
	public void showFolderDetails() {
		if (hiliteDocId != null)
			selectDocument(hiliteDocId, false);
		else {
			if (detailPanel != null)
				details.removeMember(detailPanel);
			detailPanel.destroy();
			detailPanel = new FolderDetailsPanel(folder);
			details.addMember(detailPanel);
			details.redraw();
		}
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
		return mode;
	}

	public void setMode(int mode) {
		this.mode = mode;
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

		toolbar.update(document);
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

	/**
	 * Shows the documents list under the folder with the given folderId and
	 * highlights the document with the given docId
	 */
	public void onFolderSelect(long folderId, Long docId) {
		if (docId != null)
			hiliteDocId = docId;
		FolderService.Instance.get().getFolder(folderId, false, new AsyncCallback<GUIFolder>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIFolder folder) {
				updateListingPanel(folder, hiliteDocId, max, 1, mode);
			}
		});
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		this.folder = folder;
		refresh();
	}
}