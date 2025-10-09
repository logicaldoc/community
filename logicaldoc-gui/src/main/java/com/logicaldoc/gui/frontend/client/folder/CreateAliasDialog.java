package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * This is the form used to copy a folder into another path
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class CreateAliasDialog extends Dialog {
	public CreateAliasDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("createalias"));
		setWidth(470);
		setHeight(280);
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

		HLayout buttons = new HLayout();
		buttons.setWidth100();
		buttons.setHeight(30);

		Button create = new Button(I18N.message("create"));
		create.setAutoFit(true);
		create.setMargin(1);
		create.addClickHandler(event -> {
			createAlias(folders.getSelectedRecord().getAttributeAsLong("folderId"));
			destroy();
		});

		buttons.setMembers(create);
		content.setMembers(folders, buttons);
		addItem(content);
	}

	/**
	 * Creates an alias in the currently selected folder
	 * 
	 * @param referencedFolderId The original folder to reference
	 */
	public void createAlias(long referencedFolderId) {
		final TreeNode parent = FolderNavigator.get().getSelectedRecord();
		FolderService.Instance.get().createAlias(
				parent.getAttributeAsLong(com.logicaldoc.gui.frontend.client.folder.browser.FolderTree.FOLDER_ID),
				referencedFolderId, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIFolder ret) {
						if (parent != null)
							FolderNavigator.get().getTree().reloadChildren(parent);
					}
				});
	}
}