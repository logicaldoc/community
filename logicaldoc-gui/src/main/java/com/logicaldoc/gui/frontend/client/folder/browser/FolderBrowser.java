package com.logicaldoc.gui.frontend.client.folder.browser;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.events.CellClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * A folders tree with pagination capabilities
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class FolderBrowser extends VLayout {

	private FolderTree folderTree;

	public FolderBrowser() {
		FolderCursor cursor = null;
		if (Session.get().isFolderPagination()) {
			cursor = new FolderCursor();
			cursor.setWidth(1);

			HLayout cursorLayout = new HLayout();
			cursorLayout.setStyleName("folder-cursor");
			cursorLayout.setWidth100();
			cursorLayout.setAutoHeight();

			HLayout spacer = new HLayout();
			spacer.setWidth("*");
			spacer.setHeight(1);

			cursorLayout.setMembers(cursor, spacer);
			addMember(cursorLayout);
		}

		folderTree = new FolderTree(cursor);
		folderTree.setWidth100();
		folderTree.addCellContextClickHandler(contextClick -> {
			showContextMenu();
			contextClick.cancel();
		});
		addMember(folderTree);
	}

	private void showContextMenu() {
		MenuItem reload = new MenuItem();
		reload.setTitle(I18N.message("reload"));
		reload.addClickHandler(click -> folderTree.reloadChildren());
		Menu contextMenu = new Menu();
		contextMenu.setItems(reload);
		contextMenu.showContextMenu();
	}

	public void openFolder(long folderId) {
		folderTree.openFolder(folderId, false);
	}

	public void addCellClickHandler(CellClickHandler handler) {
		folderTree.addCellClickHandler(handler);
	}

	public long getSelectedFolderId() {
		return folderTree.getSelectedFolderId();
	}

	public TreeNode getSelectedRecord() {
		return folderTree.getSelectedRecord();
	}

	public FolderTree getFolderTree() {
		return folderTree;
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