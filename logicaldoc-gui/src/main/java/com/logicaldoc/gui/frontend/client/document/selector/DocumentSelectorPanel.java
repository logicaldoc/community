package com.logicaldoc.gui.frontend.client.document.selector;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.document.DocumentsListPanel;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderBrowser;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * A panel with folder browser and document selection
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class DocumentSelectorPanel extends HLayout {

	private FolderBrowser folders;

	private Canvas documents = new VLayout();

	public DocumentSelectorPanel() {
		this(null);
	}

	public DocumentSelectorPanel(Long defaultFolderId) {
		setWidth100();

		documents = new Label("&nbsp;" + I18N.message("selectfolder"));
		documents.setAlign(Alignment.CENTER);
		documents.setWidth100();
		documents.setShowResizeBar(true);

		prepareFolderBrowser(defaultFolderId);
		setMembers(folders, documents);
	}

	private void prepareFolderBrowser(Long defaultFolderId) {
		folders = new FolderBrowser();
		folders.setWidth(250);
		folders.setShowResizeBar(true);
		folders.addCellClickHandler(event -> onFolderSelected(folders.getSelectedFolderId()));
		folders.openFolder(
				defaultFolderId != null ? defaultFolderId : FolderController.get().getCurrentFolder().getId());
		onFolderSelected(defaultFolderId != null ? defaultFolderId : FolderController.get().getCurrentFolder().getId());
	}

	private void onFolderSelected(long folderId) {
		FolderService.Instance.get().getFolder(folderId, false, true, Session.get().isFolderPagination(),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIFolder folder) {
						removeMember(documents);
						documents = new DocumentSelectorDocumentsPanel(folder);
						addMember(documents);
					}
				});
	}

	public List<GUIDocument> getSelection() {
		return ((DocumentsListPanel) documents).getGrid().getSelectedDocuments();
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