package com.logicaldoc.gui.frontend.client.search;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
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
import com.smartgwt.client.widgets.events.VisibilityChangedEvent;
import com.smartgwt.client.widgets.events.VisibilityChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel is used to show the user a list of search results
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class SearchPanel extends HLayout implements SearchObserver, DocumentObserver {

	private Layout content = new VLayout();

	private Layout details = new VLayout();

	private HitsListPanel listingPanel;

	private Canvas detailPanel;

	private static SearchPanel instance;

	private VLayout body = new VLayout();

	private SearchPreviewPanel previewPanel;

	private SearchPanel() {
		Search.get().addObserver(this);

		DocumentController.get().addObserver(this);

		setWidth100();
		setOverflow(Overflow.HIDDEN);

		// Prepare the collapsable menu
		SearchMenu searchMenu = SearchMenu.get();
		searchMenu.setWidth(350);
		searchMenu.setShowResizeBar(true);

		// Initialize the listing panel as placeholder
		listingPanel = new HitsListPanel();
		content.setAlign(Alignment.CENTER);
		content.setHeight("51%");
		content.setShowResizeBar(true);
		content.addMember(listingPanel);

		// Add a details panel under the listing one
		detailPanel = new Label("&nbsp;" + I18N.message("selectahit"));
		details.setAlign(Alignment.CENTER);
		details.addMember(detailPanel);

		body.addMember(content);
		body.addMember(details);
		body.setShowResizeBar(true);
		body.setResizeBarTarget("next");

		previewPanel = new SearchPreviewPanel();
		previewPanel.addVisibilityChangedHandler(new VisibilityChangedHandler() {

			@Override
			public void onVisibilityChanged(VisibilityChangedEvent event) {
				if (detailPanel instanceof DocumentDetailsPanel)
					previewPanel.setDocument(((DocumentDetailsPanel) detailPanel).getDocument());
			}
		});

		setMembers(searchMenu, body, previewPanel);

		setShowEdges(false);
	}

	public static SearchPanel get() {
		if (instance == null)
			instance = new SearchPanel();
		return instance;
	}

	public void onSelectedDocumentHit(long id) {
		if (id > 0) {
			DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIDocument result) {
					Session.get().setCurrentDocument(result);
				}
			});
		} else
			onDocumentSelected(null);
	}

	public void onSelectedFolderHit(long id) {
		if (details.contains(detailPanel))
			details.removeMember(detailPanel);
		detailPanel.destroy();
		if (id > 0) {
			FolderService.Instance.get().getFolder(id, true, new AsyncCallback<GUIFolder>() {

				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIFolder fld) {
					detailPanel = new FolderDetailsPanel(fld);
					details.addMember(detailPanel);
				}
			});
		} else {
			detailPanel = new Label("&nbsp;" + I18N.message("selectahit"));
			details.addMember(detailPanel);
		}
	}

	@Override
	public void onSearchArrived() {
		onSelectedDocumentHit(-1);
		previewPanel.reset();
	}

	@Override
	public void onOptionsChanged(GUISearchOptions newOptions) {

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
		return ((HitsListPanel) listingPanel).getList();
	}

	public DocumentsGrid getDocumentsGrid() {
		DocumentsGrid grid = getGrid();
		if (grid instanceof DocumentsListGrid)
			return (DocumentsGrid) grid;
		else
			return null;
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		if (!MainPanel.get().isOnSearchTab())
			return;

		if (document == null || document.getId() < 1 || !(detailPanel instanceof DocumentDetailsPanel)) {
			if (details.contains(detailPanel))
				details.removeMember(detailPanel);
			detailPanel.destroy();
			if (document == null || document.getId() < 1) {
				detailPanel = new Label("&nbsp;" + I18N.message("selectahit"));
				details.addMember(detailPanel);
				return;
			}
		}

		if (detailPanel instanceof DocumentDetailsPanel) {
			((DocumentDetailsPanel) detailPanel).setDocument(document);

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
		previewPanel.setDocument(document);
	}

	public SearchPreviewPanel getPreviewPanel() {
		return previewPanel;
	}

	@Override
	public void onDocumentsDeleted(GUIDocument[] documents) {
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

	}
}