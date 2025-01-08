package com.logicaldoc.gui.frontend.client.search;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.DocumentObserver;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.FolderObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.document.DocumentDetailsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.folder.FolderDetailsPanel;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel is used to show the user a list of search results
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchPanel extends HLayout implements SearchObserver, DocumentObserver, FolderObserver {

	private static final String NBSP = "&nbsp;";

	private static final String SELECTAHIT = "selectahit";

	private Layout listing = new VLayout();

	private Layout details = new VLayout();

	private HitsListPanel listingPanel;

	private Canvas detailPanel;

	private static SearchPanel instance;

	private VLayout body = new VLayout();

	private SearchPreviewPanel previewPanel;

	private boolean drawn = false;

	private SearchToolbar toolbar;

	private SearchPanel() {
		setWidth100();
		setOverflow(Overflow.HIDDEN);
		setShowEdges(false);

		Search.get().addObserver(this);
		DocumentController.get().addObserver(this);
		FolderController.get().addObserver(this);
	}

	public static SearchPanel get() {
		if (instance == null)
			instance = new SearchPanel();
		return instance;
	}

	@Override
	public void onDraw() {
		if (drawn)
			return;
		drawn = true;

		// Prepare the collapsable menu
		SearchMenu searchMenu = SearchMenu.get();
		searchMenu.setWidth(350);
		searchMenu.setShowResizeBar(true);

		// Initialize the listing panel as placeholder
		listingPanel = new HitsListPanel();
		listing.setAlign(Alignment.CENTER);
		listing.setHeight("51%");
		listing.setShowResizeBar(true);
		listing.addMember(listingPanel);

		// Add a details panel under the listing one
		detailPanel = new Label(NBSP + I18N.message(SELECTAHIT));
		details.setAlign(Alignment.CENTER);
		details.addMember(detailPanel);

		// The two rows: listing and details
		VLayout listingAndDetails = new VLayout();
		listingAndDetails.setMembers(listing, details);
		listingAndDetails.setShowResizeBar(true);
		listingAndDetails.setResizeBarTarget("next");

		previewPanel = new SearchPreviewPanel();
		previewPanel.addVisibilityChangedHandler(event -> {
			if (detailPanel instanceof DocumentDetailsPanel docDetails)
				previewPanel.setDocument(docDetails.getDocument());
		});

		// The listing plus the preview
		HLayout bodyPanel = new HLayout();
		bodyPanel.setWidth100();
		bodyPanel.setMembers(listingAndDetails, previewPanel);

		toolbar = new SearchToolbar(listingPanel);
		body.setMembers(toolbar, bodyPanel);

		setMembers(searchMenu, body);
	}

	public void onSelectedDocumentHit(long id) {
		if (id > 0) {
			DocumentService.Instance.get().getById(id, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument result) {
					DocumentController.get().setCurrentDocument(result);
				}
			});
		} else
			onDocumentSelected(null);
	}

	public void onSelectedFolderHit(long id) {
		if (id > 0) {
			FolderService.Instance.get().getFolder(id, true, false, false, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIFolder fld) {
					if (detailPanel instanceof FolderDetailsPanel folderDetails) {
						folderDetails.setFolder(fld);
					} else {
						details.removeMember(detailPanel);
						detailPanel = new FolderDetailsPanel(fld);
						details.addMember(detailPanel);
					}
				}
			});
		} else {
			detailPanel = new Label(NBSP + I18N.message(SELECTAHIT));
			details.addMember(detailPanel);
		}
	}

	@Override
	public void onSearchArrived() {
		onSelectedDocumentHit(-1);
		previewPanel.reset();
		enableAll();
	}

	@Override
	public void onOptionsChanged(GUISearchOptions newOptions) {
		// Nothing to do
	}

	public boolean isMenuOpened() {
		return SearchMenu.get().getWidth() > 2;
	}

	public void toggleMenu() {
		if (SearchMenu.get().getWidth() > 2)
			SearchMenu.get().setWidth(0);
		else
			SearchMenu.get().setWidth(350);
	}

	public DocumentsGrid getGrid() {
		return listingPanel.getList();
	}

	public DocumentsGrid getDocumentsGrid() {
		DocumentsGrid grid = getGrid();
		if (grid instanceof DocumentsListGrid)
			return grid;
		else
			return null;
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		if (!MainPanel.get().isOnSearchTab())
			return;

		if (document == null || document.getId() < 1 || !(detailPanel instanceof DocumentDetailsPanel)) {
			if (Boolean.TRUE.equals(details.contains(detailPanel)))
				details.removeMember(detailPanel);
			detailPanel.destroy();
			if (document == null || document.getId() < 1) {
				detailPanel = new Label(NBSP + I18N.message(SELECTAHIT));
				details.addMember(detailPanel);
				return;
			}
		}

		if (detailPanel instanceof DocumentDetailsPanel docDetails) {
			docDetails.setDocument(document);

		} else {
			detailPanel = new DocumentDetailsPanel();
			((DocumentDetailsPanel) detailPanel).setDocument(document);
			details.addMember(detailPanel);
		}
		details.redraw();

		previewPanel.setDocument(document);
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		if (getGrid().getSelectedDocument() != null && getGrid().getSelectedDocument().getId() == document.getId()) {
			previewPanel.setDocument(document);
			onDocumentCancelEditing(document);
		}
	}

	public SearchPreviewPanel getPreviewPanel() {
		return previewPanel;
	}

	@Override
	public void onDocumentsDeleted(List<GUIDocument> documents) {
		previewPanel.reset();
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		// Nothing to do
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
		DocumentsGrid grid = getGrid();
		if (grid.getSelectedDocument().getId() == document.getId()) {
			disableAll();
		}
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		DocumentsGrid grid = getGrid();
		if (grid.getSelectedDocument().getId() == document.getId()) {
			enableAll();
		}
	}

	private void enableAll() {
		if (listingPanel != null)
			listingPanel.enable();
		if (toolbar != null)
			toolbar.enable();
		SearchMenu.get().enable();
	}

	private void disableAll() {
		if (listingPanel != null)
			listingPanel.disable();
		if (toolbar != null)
			toolbar.disable();
		SearchMenu.get().disable();
	}

	@Override
	public void destroy() {
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

	public HitsListPanel getListingPanel() {
		return listingPanel;
	}

	public String getDocsGridLayout() {
		if (listingPanel != null) {
			if (listingPanel.getGrid() instanceof SearchHitsGrid)
				return listingPanel.getGridLayout();
			else
				return null;
		} else
			return null;
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		// Noting to do
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		// Noting to do
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		// Noting to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		// Noting to do
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		// Noting to do
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		DocumentsGrid grid = getGrid();
		if (grid.getSelectedDocument().getId() == folder.getId()) {
			disableAll();
		}
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		DocumentsGrid grid = getGrid();
		if (grid.getSelectedDocument().getId() == folder.getId()) {
			enableAll();
		}
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