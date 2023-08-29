package com.logicaldoc.gui.frontend.client.document.selector;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
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
		setWidth100();

		documents = new Label("&nbsp;" + I18N.message("selectfolder"));
		documents.setAlign(Alignment.CENTER);
		documents.setWidth100();
		documents.setShowResizeBar(true);
		
		prepareFolderBrowser();
		setMembers(folders, documents);
	}

	private void prepareFolderBrowser() {
		folders = new FolderBrowser();
		folders.setWidth(250);
		folders.setShowResizeBar(true);
		folders.addCellClickHandler(event -> FolderService.Instance.get().getFolder(folders.getSelectedFolderId(),
				false, false, Session.get().isFolderPagination(), new AsyncCallback<GUIFolder>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder folder) {
						removeMember(documents);
						documents = new DocumentsListPanel(folder);
						documents.setCanDrag(false);
						addMember(documents);
					}
				}));
	}

	public GUIDocument[] getSelection() {
		return ((DocumentsListPanel) documents).getGrid().getSelectedDocuments();
	}
}