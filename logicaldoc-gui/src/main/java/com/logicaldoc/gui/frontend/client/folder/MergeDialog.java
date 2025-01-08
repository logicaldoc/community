package com.logicaldoc.gui.frontend.client.folder;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderBrowser;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * This is the form used to copy some folders into another target folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class MergeDialog extends Dialog {
	private static final String MERGE = "merge";

	public MergeDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(MERGE));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setWidth(440);
		setHeight(500);

		final FolderBrowser folders = new FolderBrowser();
		folders.setWidth100();
		folders.setHeight100();
		folders.setMinHeight(300);

		ToolStripButton merge = new ToolStripButton(I18N.message(MERGE));
		merge.setAutoFit(true);
		merge.setMargin(1);
		merge.addClickHandler(event -> {
			List<Long> selectedIds = FolderNavigator.get().getSelectedIds();
			String label = FolderNavigator.get().getSelectedRecord().getAttributeAsString("name");
			if (!selectedIds.isEmpty())
				label = selectedIds.size() + " " + I18N.message("folders").toLowerCase();

			LD.ask(I18N.message(MERGE),
					I18N.message("mergeask", label, folders.getSelectedRecord().getAttributeAsString("name")), yes -> {
						if (Boolean.TRUE.equals(yes))
							merge(folders.getSelectedRecord().getAttributeAsLong(
									com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID));
						destroy();
					});
		});

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();
		buttons.addButton(merge);

		addMember(folders);
		addMember(buttons);
	}

	/**
	 * Merges the currently selected folders to a target folder
	 * 
	 * @param targetFolderId The target folder
	 */
	private void merge(long targetFolderId) {
		List<Long> ids = FolderNavigator.get().getSelectedIds();
		for (Long id : ids) {
			TreeNode node = FolderNavigator.get().getTree()
					.find(com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID, (Object) id);
			FolderNavigator.get().getTree().remove(node);
		}

		LD.contactingServer();
		FolderService.Instance.get().merge(ids, targetFolderId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void ret) {
				LD.clearPrompt();
				TreeNode target = FolderNavigator.get().getTree().find(
						com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID,
						Long.toString(targetFolderId));
				if (target != null)
					FolderNavigator.get().getTree().reloadChildren(target);
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