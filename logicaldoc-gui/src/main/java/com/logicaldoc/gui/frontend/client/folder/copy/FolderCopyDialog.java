package com.logicaldoc.gui.frontend.client.folder.copy;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridComponent;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * This is the form used to copy a folder into another path
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class FolderCopyDialog extends Dialog {
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
		setMembersMargin(1);

		TreeGrid folders = new FolderTree();
		folders.setWidth100();
		folders.setHeight100();
		folders.setMinHeight(300);
		folders.setMinWidth(400);

		long[] selectedSourceIds = FolderNavigator.get().getSelectedIds();

		final boolean securityOptionEnabled = "true"
				.equals(Session.get().getInfo().getConfig("gui.security.inheritoption"));

		final DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setStyleName("sectionHeaderopened");

		TextItem name = ItemFactory.newTextItem("name", "newname",
				FolderNavigator.get().getSelectedRecord().getAttributeAsString("name"));
		name.setHidden(selectedSourceIds.length > 1);

		SelectItem securityOption = ItemFactory.newFolderSecurityOption("security", "security");
		securityOption.setHidden(!securityOptionEnabled);

		CheckboxItem foldersOnly = ItemFactory.newCheckbox("foldersOnly", "copyfoldersonly");
		foldersOnly.setValue(false);

		ButtonItem copy = new ButtonItem(I18N.message("copy"));
		copy.setAutoFit(true);
		copy.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				if (!form.validate())
					return;

				if (folders.getSelectedRecord() == null
						|| folders.getSelectedRecord().getAttributeAsString("folderId") == null) {
					GuiLog.warn(I18N.message("choosetargetfolder"), null);
					return;
				}

				long tagetFolderId = Long.parseLong(folders.getSelectedRecord().getAttributeAsString("folderId"));

				if (selectedSourceIds.length == 1) {
					FolderService.Instance.get().getFolder(selectedSourceIds[0], false, false, false,
							new AsyncCallback<GUIFolder>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIFolder sourceFolder) {
									sourceFolder.setName(form.getValueAsString("name"));
									sourceFolder.setPermissions(new String[] { "read", "write" });

									FolderCopyDetailsDialog dialog = new FolderCopyDetailsDialog(sourceFolder,
											tagetFolderId, form.getValueAsString("security"),
											"true".equals(form.getValueAsString("foldersOnly")));
									dialog.show();
									hide();
									destroy();
								}
							});
				} else {
					String label = FolderNavigator.get().getSelectedRecord().getAttributeAsString("name");
					label = selectedSourceIds.length + " " + I18N.message("folders").toLowerCase();

					LD.ask(I18N.message("copy"),
							I18N.message("copyask",
									new String[] { label, folders.getSelectedRecord().getAttributeAsString("name") }),
							new BooleanCallback() {

								@Override
								public void execute(Boolean value) {
									if (value) {
										FolderNavigator.get().copyTo(tagetFolderId,
												"true".equals(form.getValueAsString("foldersOnly")),
												!securityOptionEnabled ? "inheritparentsec"
														: form.getValueAsString("security"));
										hide();
										destroy();
									}
								}
							});
				}
			}
		});

		if (securityOptionEnabled)
			form.setItems(name, foldersOnly, securityOption, copy);
		else
			form.setItems(name, copy);

		folders.setGridComponents(new Object[] { ListGridComponent.HEADER, ListGridComponent.BODY, form });

		SectionStackSection section = new SectionStackSection(I18N.message("targetfolder"));
		section.setCanCollapse(false);
		section.setExpanded(true);
		section.setItems(folders);

		SectionStack stack = new SectionStack();
		stack.setWidth100();
		stack.setHeight100();
		stack.setSections(section);

		addItem(stack);
	}
}