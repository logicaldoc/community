package com.logicaldoc.gui.frontend.client.account;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.MultipleUpload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the dialog for uploading a new certificate file and related private
 * key
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class CertificateUploadDialog extends Window {

	private DynamicForm form;

	private MultipleUpload uploader;

	private IButton sendButton;

	public CertificateUploadDialog() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadyourowncert"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth(520);
		setHeight(620);
		centerInPage();

		sendButton = new IButton(I18N.message("submit"));
		sendButton.addClickHandler(event -> onSubmit());
		sendButton.setDisabled(true);

		prepareForm();

		uploader = new MultipleUpload(sendButton, "dropyourcerthere");
		uploader.setFileTypes("*.crt");
		uploader.setWidth("520");
		uploader.setMaxUploads(1);

		HLayout spacer1 = new HLayout();
		spacer1.setWidth100();
		spacer1.setHeight(6);

		HLayout spacer2 = new HLayout();
		spacer2.setWidth100();
		spacer2.setHeight(6);
		
		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();
		layout.addMember(spacer1);
		layout.addMember(uploader);
		layout.addMember(spacer2);
		layout.addMember(form);
		layout.addMember(sendButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(event -> DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				destroy();
			}
		}));

		addItem(layout);

		cleanUploadFolder();
	}

	private void cleanUploadFolder() {
		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				// Nothing to do
			}

			@Override
			public void onSuccess(Void result) {
				// Nothing to do
			}
		});
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setColWidths("1px, 100%");
		form.setTitleOrientation(TitleOrientation.TOP);

		TextAreaItem privateKey = ItemFactory.newTextAreaItem("privatekey", null);
		privateKey.setRequired(true);
		privateKey.setMinHeight(400);
		privateKey.setWidth(500);

		form.setItems(privateKey);
	}

	public void onSubmit() {
		if (Boolean.FALSE.equals(form.validate()))
			return;

		if (uploader.getUploadedFiles().isEmpty()) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		LD.contactingServer();
		SignService.Instance.get().importCertificate(form.getValueAsString("privatekey"), new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				LD.clearPrompt();
				cleanUploadFolder();
				SecurityService.Instance.get().getUser(Session.get().getUser().getId(),
						new AsyncCallback<GUIUser>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIUser user) {
								Session.get().setUser(user);
								UserController.get().changed(user);
								destroy();
							}
						});
			}
		});
	}
}