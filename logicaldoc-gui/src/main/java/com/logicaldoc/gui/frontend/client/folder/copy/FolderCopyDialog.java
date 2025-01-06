package com.logicaldoc.gui.frontend.client.folder.copy;

import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.GUIAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderBrowser;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * This is the form used to copy a folder into another path
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class FolderCopyDialog extends Dialog {
	private static final String FOLDERS_ONLY = "foldersOnly";

	private static final String SECURITY = "security";

	public FolderCopyDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("copyfolder"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setWidth(440);
		setHeight(600);

		FolderBrowser folders = new FolderBrowser();
		folders.setWidth100();
		folders.setHeight100();

		List<Long> selectedSourceIds = FolderNavigator.get().getSelectedIds();

		final boolean securityOptionEnabled = Session.get().getConfigAsBoolean("gui.security.inheritoption");

		final DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setStyleName("sectionHeaderopened");

		TextItem name = ItemFactory.newTextItem("name", "newname",
				FolderNavigator.get().getSelectedRecord().getAttributeAsString("name"));
		name.setHidden(selectedSourceIds.size() > 1);

		SelectItem securityOption = ItemFactory.newFolderSecurityOption(SECURITY);
		securityOption.setHidden(!securityOptionEnabled);

		CheckboxItem foldersOnly = ItemFactory.newCheckbox(FOLDERS_ONLY, "copyfoldersonly");
		foldersOnly.setValue(false);

		ButtonItem copy = prepareCopyButton(folders.getFolderTree(), selectedSourceIds, securityOptionEnabled, form);

		if (securityOptionEnabled)
			form.setItems(name, foldersOnly, securityOption, copy);
		else
			form.setItems(name, copy);

		addMember(folders);
		addMember(form);
	}

	private ButtonItem prepareCopyButton(TreeGrid folders, List<Long> selectedSourceIds,
			final boolean securityOptionEnabled, final DynamicForm form) {
		ButtonItem copy = new ButtonItem(I18N.message("copy"));
		copy.setAutoFit(true);
		copy.addClickHandler((com.smartgwt.client.widgets.form.fields.events.ClickEvent event) -> {
			if (!form.validate())
				return;

			if (folders.getSelectedRecord() == null
					|| folders.getSelectedRecord().getAttributeAsString("folderId") == null) {
				GuiLog.warn(I18N.message("choosetargetfolder"), null);
				return;
			}

			long tagetFolderId = Long.parseLong(folders.getSelectedRecord().getAttributeAsString("folderId"));

			if (selectedSourceIds.size() == 1) {
				copySingleFolder(selectedSourceIds.get(0), form, tagetFolderId);
			} else {
				copyMultipleFolders(folders, selectedSourceIds, securityOptionEnabled, form, tagetFolderId);
			}
		});
		return copy;
	}

	private void copyMultipleFolders(TreeGrid folders, List<Long> selectedSourceIds,
			final boolean securityOptionEnabled, final DynamicForm form, long tagetFolderId) {
		String label = selectedSourceIds.size() + " " + I18N.message("folders").toLowerCase();

		LD.ask(I18N.message("copy"),
				I18N.message("copyask", Arrays.asList(label, folders.getSelectedRecord().getAttributeAsString("name"))),
				yes -> {
					if (Boolean.TRUE.equals(yes)) {
						copy(tagetFolderId, Boolean.valueOf(form.getValueAsString(FOLDERS_ONLY)),
								!securityOptionEnabled ? "inheritparentsec" : form.getValueAsString(SECURITY));
						hide();
						destroy();
					}
				});
	}

	private void copySingleFolder(long selectedSourceId, final DynamicForm form, long tagetFolderId) {
		FolderService.Instance.get().getFolder(selectedSourceId, false, false, false, new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(GUIFolder sourceFolder) {
				sourceFolder.setName(form.getValueAsString("name"));
				sourceFolder.setAllowedPermissions(new GUIAccessControlEntry(GUIAccessControlEntry.PERMISSION_READ,
						GUIAccessControlEntry.PERMISSION_WRITE));

				new FolderCopyDetailsDialog(sourceFolder, tagetFolderId, form.getValueAsString(SECURITY),
						Boolean.valueOf(form.getValueAsString(FOLDERS_ONLY))).show();
				hide();
				destroy();
			}
		});
	}

	/**
	 * Copies the currently selected folders to the new parent folder
	 * 
	 * @param targetFolderId identifier of the parent folder
	 * @param foldersOnly to create just the folders
	 * @param securityOption how to setup the security for the new folder'none',
	 *        'inherit' or 'replicate'
	 */
	private void copy(long targetFolderId, boolean foldersOnly, String securityOption) {
		final TreeNode target = FolderNavigator.get().getTree().findById(Long.toString(targetFolderId));

		LD.contactingServer();
		FolderService.Instance.get().copyFolders(FolderNavigator.get().getSelectedIds(), targetFolderId, foldersOnly,
				securityOption, null, new GUIAsyncCallback<>() {
					@Override
					public void onSuccess(Void ret) {
						LD.clearPrompt();
						if (target != null)
							FolderNavigator.get().reload();
					}
				});
	}
}