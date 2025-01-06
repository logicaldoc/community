package com.logicaldoc.gui.frontend.client.account.certificate;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.GUIAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the dialog for uploading a new certificate file and related private
 * key
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class OwnCertificateDialog extends Window {

	private static final String PRIVATEKEY = "privatekey";

	private static final String CERTIFICATE = "certificate";

	private DynamicForm form;

	private IButton submitButton;

	private static final int TEXTAREA_HEIGHT = 120;

	private static final int TEXTAREA_WIDTH = 530;

	public OwnCertificateDialog() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadyourowncert"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		submitButton = new IButton(I18N.message("submit"));
		submitButton.addClickHandler(event -> onSubmit());

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();
		layout.addMember(form);
		layout.addMember(submitButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(event -> DocumentService.Instance.get().cleanUploadedFileFolder(new GUIAsyncCallback<>() {

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
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<>() {

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

		TextAreaItem certificateItem = ItemFactory.newTextAreaItem(CERTIFICATE + ".crt", CERTIFICATE, null);
		certificateItem.setWrapTitle(false);
		certificateItem.setRequired(true);
		certificateItem.setColSpan(2);
		certificateItem.setWidth(TEXTAREA_WIDTH);
		certificateItem.setHeight(TEXTAREA_HEIGHT);
		certificateItem.setIcons(new CertificateUploadFormItemIcon("uploadcertificate"));

		TextAreaItem privateKeyItem = ItemFactory.newTextAreaItem(PRIVATEKEY, PRIVATEKEY, null);
		privateKeyItem.setWrapTitle(false);
		privateKeyItem.setRequired(true);
		privateKeyItem.setColSpan(2);
		privateKeyItem.setWidth(TEXTAREA_WIDTH);
		privateKeyItem.setHeight(TEXTAREA_HEIGHT);
		privateKeyItem.setIcons(new CertificateUploadFormItemIcon("uploadprivatekey"));

		form.setItems(certificateItem, privateKeyItem);
	}

	public void onSubmit() {
		if (Boolean.FALSE.equals(form.validate()))
			return;

		LD.contactingServer();
		SignService.Instance.get().importCertificate(form.getValueAsString(CERTIFICATE + ".crt"),
				form.getValueAsString(PRIVATEKEY), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						LD.clearPrompt();
						cleanUploadFolder();
						SecurityService.Instance.get().getUser(Session.get().getUser().getId(), new GUIAsyncCallback<>() {

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