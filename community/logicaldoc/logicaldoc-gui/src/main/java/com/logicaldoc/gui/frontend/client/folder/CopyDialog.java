package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * This is the form used to copy a folder into another path
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class CopyDialog extends Dialog {
	public CopyDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("copy"));
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

		final boolean inheritOptionEnabled = "true".equals(Session.get().getInfo()
				.getConfig("gui.security.inheritoption"));

		final DynamicForm form = new DynamicForm();
		CheckboxItem inheritSecurity = new CheckboxItem();
		inheritSecurity.setName("inheritSecurity");
		inheritSecurity.setTitle(I18N.message("inheritparentsec"));
		inheritSecurity.setValue(Session.get().getInfo().getConfig("gui.security.inheritoption.default"));
		form.setItems(inheritSecurity);

		Button copy = new Button(I18N.message("copy"));
		copy.setAutoFit(true);
		copy.setMargin(1);
		copy.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				long[] selectedIds = FolderNavigator.get().getSelectedIds();
				String label = FolderNavigator.get().getSelectedRecord().getAttributeAsString("name");
				if (selectedIds.length > 1)
					label = selectedIds.length + " " + I18N.message("folders").toLowerCase();

				LD.ask(I18N.message("copy"),
						I18N.message("copyask",
								new String[] { label, folders.getSelectedRecord().getAttributeAsString("name") }),
						new BooleanCallback() {

							@Override
							public void execute(Boolean value) {
								if (value) {
									FolderNavigator.get()
											.copyTo(Long.parseLong(folders.getSelectedRecord().getAttributeAsString(
													"folderId")),
													false,
													!inheritOptionEnabled
															|| "true".equals(form.getValueAsString("inheritSecurity")));
								}
								destroy();
							}
						});
			}
		});

		Button copyFolders = new Button(I18N.message("copyfoldersonly"));
		copyFolders.setAutoFit(true);
		copyFolders.setMargin(1);
		copyFolders.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				LD.ask(I18N.message("copy"),
						I18N.message("copyask", new String[] {
								FolderNavigator.get().getSelectedRecord().getAttributeAsString("name"),
								folders.getSelectedRecord().getAttributeAsString("name") }), new BooleanCallback() {

							@Override
							public void execute(Boolean value) {
								if (value) {
									FolderNavigator.get()
											.copyTo(Long.parseLong(folders.getSelectedRecord().getAttributeAsString(
													"folderId")),
													true,
													!inheritOptionEnabled
															|| "true".equals(form.getValueAsString("inheritSecurity")));
								}
								destroy();
							}
						});
			}
		});

		if (inheritOptionEnabled)
			buttons.setMembers(copy, copyFolders, form);
		else
			buttons.setMembers(copy, copyFolders);

		content.setMembers(folders, buttons);
		addItem(content);
	}
}