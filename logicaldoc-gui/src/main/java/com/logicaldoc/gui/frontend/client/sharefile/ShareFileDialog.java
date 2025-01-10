package com.logicaldoc.gui.frontend.client.sharefile;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.SearchPanel;
import com.logicaldoc.gui.frontend.client.services.ShareFileService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * This is the form used to select an element in ShareFile
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class ShareFileDialog extends Dialog {

	private TreeGrid tree = null;

	private boolean export = false;

	public ShareFileDialog(boolean export) {
		super();
		this.export = export;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("sharefile"));
		setWidth(350);
		setHeight(300);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		VLayout content = new VLayout();
		content.setTop(10);
		content.setWidth100();
		content.setHeight100();
		content.setMembersMargin(3);

		tree = new ShareFileTree(export);
		tree.setWidth100();
		tree.setHeight100();

		VLayout buttons = new VLayout();
		buttons.setWidth100();
		buttons.setHeight(30);

		Button select = new Button(I18N.message("select"));
		select.setAutoFit(true);
		select.setMargin(1);
		select.addClickHandler(event -> {
			if (ShareFileDialog.this.export)
				onExport();
			else
				onImport();
		});

		buttons.setMembers(select);

		content.setMembers(tree, buttons);
		addItem(content);
	}

	private void onExport() {
		final Record selection = tree.getSelectedRecord();
		if (selection == null)
			return;

		final List<Long> docIds = MainPanel.get().isOnDocumentsTab()
				? DocumentsPanel.get().getDocumentsGrid().getSelectedIds()
				: SearchPanel.get().getDocumentsGrid().getSelectedIds();

		SC.ask(docIds.isEmpty() ? I18N.message("exportdirtosfile", FolderController.get().getCurrentFolder().getName())
				: I18N.message("exportdocstosfile"), choice -> {
					if (choice.booleanValue()) {
						String targetId = selection.getAttributeAsString("iid");
						List<Long> folderIds = new ArrayList<>();
						if (docIds.isEmpty() && FolderController.get().getCurrentFolder() != null)
							folderIds.add(FolderController.get().getCurrentFolder().getId());

						LD.contactingServer();
						ShareFileService.Instance.get().exportDocuments(targetId, folderIds, docIds,
								new DefaultAsyncCallback<>() {
									@Override
									public void onSuccess(Boolean result) {
										LD.clearPrompt();
										if (result.booleanValue()) {
											SC.say(I18N.message("sfileexportok"));
											ShareFileDialog.this.destroy();
										} else
											SC.say(I18N.message("sfileexportko"));
									}
								});
					}
				});
	}

	private void onImport() {
		final Record[] selection = tree.getSelectedRecords();
		if (selection == null)
			return;

		List<String> ids = new ArrayList<>();
		for (int i = 0; i < selection.length; i++)
			ids.add(selection[i].getAttributeAsString("iid"));

		SC.ask(I18N.message("importfromsfile", FolderController.get().getCurrentFolder().getName()), choice -> {
			if (choice.booleanValue()) {
				ShareFileDialog.this.destroy();
				LD.contactingServer();
				ShareFileService.Instance.get().importDocuments(FolderController.get().getCurrentFolder().getId(), ids,
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Integer count) {
								LD.clearPrompt();
								FolderNavigator.get().reload();
								SC.say(I18N.message("importeddocs2", count.toString()));
							}
						});
			}
		});
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