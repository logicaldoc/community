package com.logicaldoc.gui.frontend.client.personal.contacts;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SubmitItem;

/**
 * This popup window is used to upload a new contacts file to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0.1
 */
public class ContactsUploader extends Window {
	private SubmitItem sendButton;

	private Upload uploader;

	private ValuesManager vm;

	private DynamicForm form;

	public ContactsUploader() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadcontacts"));
		setWidth(400);
		setHeight(125);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		sendButton = new SubmitItem();
		sendButton.setTitle(I18N.message("submit"));
		sendButton.setDisabled(true);
		sendButton.setAlign(Alignment.RIGHT);
		sendButton.addClickHandler(event -> onSubmit());

		form.setItems(sendButton);

		uploader = new Upload(sendButton);
		addItem(uploader);
		addItem(form);
	}

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		ContactsImportSettings popup = new ContactsImportSettings();
		popup.show();
		destroy();
	}
}