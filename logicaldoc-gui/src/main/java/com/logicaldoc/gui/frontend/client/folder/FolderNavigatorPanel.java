package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.frontend.client.document.DocumentsUploader;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Panle that contains both folder tool basr and folder navigator.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class FolderNavigatorPanel extends VLayout implements FolderObserver {
	private static FolderNavigatorPanel instance = null;

	private ToolStripButton newFolder;

	private ToolStripButton newWorkspace;

	private ToolStripButton addDocuments;

	public static FolderNavigatorPanel get() {
		if (instance == null) {
			instance = new FolderNavigatorPanel();
			FolderController.get().addObserver(instance);
		}
		return instance;
	}

	private FolderNavigatorPanel() {
		newFolder = new ToolStripButton(I18N.message("newfolder"));
		newFolder.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				FolderNavigator.get().onCreate(FolderNavigator.get().getSelectedIds()[0]);
			}
		});
		newFolder.setDisabled(true);

		newWorkspace = new ToolStripButton(I18N.message("newworkspace"));
		newWorkspace.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				FolderNavigator.get().onCreateWorkspace();
			}
		});
		newWorkspace.setDisabled(true);
		newWorkspace.setVisible(false);

		addDocuments = new ToolStripButton(I18N.message("adddocuments"));
		addDocuments.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocumentsUploader uploader = new DocumentsUploader();
				uploader.show();
				event.cancel();
			}
		});
		addDocuments.setDisabled(true);

		ToolStrip folderToolbar = new ToolStrip();
		folderToolbar.setWidth100();
		folderToolbar.addButton(newFolder);
		folderToolbar.addButton(addDocuments);
		folderToolbar.addButton(newWorkspace);

		setMembers(folderToolbar, FolderNavigator.get());
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		newFolder.setDisabled(!folder.hasPermission(Constants.PERMISSION_ADD));
		addDocuments.setDisabled(!folder.isWrite());
		boolean newWorkspaceDisabled = !Feature.enabled(Feature.MULTI_WORKSPACE)
				|| !Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN);
		newWorkspace.setDisabled(newWorkspaceDisabled);
		newWorkspace.setVisible(!newWorkspaceDisabled);
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
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

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		// Nothing to do
	}
}