package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the form used to create a new Folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7.1
 */
public class CreateDialog extends Dialog {

	private DynamicForm form;

	public CreateDialog(final GUIFolder folder) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(folder.getType() == 0 ? I18N.message("newfolder") : I18N.message("newworkspace"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(3);

		final boolean inheritOptionEnabled = folder.getType() == 0
				&& Session.get().getConfigAsBoolean("gui.security.inheritoption");

		form = new DynamicForm();
		form.setHeight100();
		form.setWidth100();

		CheckboxItem inheritSecurity = new CheckboxItem();
		inheritSecurity.setName("inheritSecurity");
		inheritSecurity.setTitle(I18N.message("inheritparentsec"));
		inheritSecurity.setHidden(!inheritOptionEnabled);

		TextItem name = ItemFactory.newTextItem("name",
				folder.getType() == 0 ? I18N.message("newfolder") : I18N.message("newworkspace"));
		name.setWidth(250);
		name.setRequired(true);
		name.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName()))
				onCreate(folder);
		});

		SubmitItem create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.setAutoFit(true);
		create.addClickHandler(event -> onCreate(folder));

		form.setItems(name, inheritSecurity, create);
		inheritSecurity.setValue("inherit".equals(Session.get().getConfig("gui.security.inheritoption.default")));

		VLayout content = new VLayout();
		content.setTop(10);
		content.setWidth100();
		content.setHeight100();
		content.setMembersMargin(3);
		content.addMember(form);

		addItem(content);
	}

	private void onCreate(final GUIFolder folder) {
		if (form.validate()) {
			folder.setName(form.getValueAsString("name").trim());
			FolderService.Instance.get().create(folder,
					folder.getType() == 0 && Boolean.valueOf(form.getValueAsString("inheritSecurity")),
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(GUIFolder newFolder) {
							FolderController.get().created(newFolder);
							destroy();
						}
					});
		}
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