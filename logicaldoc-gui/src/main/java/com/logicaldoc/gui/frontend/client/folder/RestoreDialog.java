package com.logicaldoc.gui.frontend.client.folder;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * This is the form used to restore a selection of documents or folders.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class RestoreDialog extends Dialog {

	protected ClickHandler handler;

	public RestoreDialog(final List<Long> docIds, final List<Long> folderIds, ClickHandler handler) {
		super();
		this.handler = handler;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("restore"));
		setWidth(250);
		setHeight(270);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(3);

		VLayout content = new VLayout();
		content.setTop(10);
		content.setWidth100();
		content.setHeight100();
		content.setMembersMargin(3);

		final TreeGrid folders = new FolderTree();
		folders.setWidth100();
		folders.setHeight100();

		VLayout buttons = new VLayout();
		buttons.setWidth100();
		buttons.setHeight(30);

		Button restore = new Button(I18N.message("restore"));
		restore.setAutoFit(true);
		restore.setMargin(1);
		restore.addClickHandler(event -> {
			if (docIds != null)
				DocumentService.Instance.get().restore(docIds,
						Long.parseLong(folders.getSelectedRecord().getAttributeAsString("folderId")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								if (RestoreDialog.this.handler != null)
									RestoreDialog.this.handler.onClick(event);
								close();
							}
						});

			if (folderIds != null)
				FolderService.Instance.get().restore(folderIds,
						Long.parseLong(folders.getSelectedRecord().getAttributeAsString("folderId")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								if (RestoreDialog.this.handler != null)
									RestoreDialog.this.handler.onClick(event);
								close();
							}
						});

			close();
		});

		buttons.setMembers(restore);

		content.setMembers(folders, buttons);
		addItem(content);
	}
}