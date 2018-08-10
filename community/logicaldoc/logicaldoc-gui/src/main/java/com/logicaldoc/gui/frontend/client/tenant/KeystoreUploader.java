package com.logicaldoc.gui.frontend.client.tenant;

import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIKeystore;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new workflow schema to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class KeystoreUploader extends Window {

	private MultiUploader uploader;

	private IButton sendButton;

	private VLayout layout = new VLayout();

	private TenantKeystorePanel keystorePanel;

	private ValuesManager vm;

	private DynamicForm form;

	public KeystoreUploader(TenantKeystorePanel keystorePanel) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		this.keystorePanel = keystorePanel;
		setTitle(I18N.message("uploadkeystore"));
		setWidth(420);
		setHeight(190);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		layout.setMembersMargin(2);
		layout.setMargin(2);

		// Create a new uploader panel and attach it to the window
		uploader = new MultiUploader();
		uploader.setMaximumFiles(1);
		uploader.setStyleName("upload");
		uploader.setFileInputPrefix("LDOC");
		uploader.setWidth("400px");
		uploader.reset();
		uploader.setHeight("40px");

		// Add a finish handler which will load the image once the upload
		// finishes
		uploader.addOnFinishUploadHandler(onFinishUploaderHandler);

		sendButton = new IButton(I18N.message("send"));
		sendButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSend();
			}
		});

		prepareForm();

		layout.addMember(form);
		layout.addMember(uploader);
		layout.addMember(sendButton);
		addItem(layout);

		// Cleanup the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
			}

			@Override
			public void onSuccess(Void result) {
			}
		});
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");
		vm = new ValuesManager();
		form.setValuesManager(vm);

		TextItem localCAalias = ItemFactory.newSimpleTextItem("localCAalias", "localcaalias",
				keystorePanel.getKeystore() != null ? keystorePanel.getKeystore().getOrganizationAlias() : null);
		localCAalias.setRequired(true);
		localCAalias.setSelectOnFocus(true);
		localCAalias.setWrapTitle(false);

		TextItem password = ItemFactory.newPasswordItem("password", "keystorepasswd",
				keystorePanel.getKeystore() != null ? keystorePanel.getKeystore().getPassword() : null);
		password.setRequired(false);
		password.setWrapTitle(false);

		form.setItems(localCAalias, password);
	}

	// Load the image in the document and in the case of success attach it to
	// the viewer
	private IUploader.OnFinishUploaderHandler onFinishUploaderHandler = new IUploader.OnFinishUploaderHandler() {
		public void onFinish(IUploader uploader) {
			if (uploader.getStatus() == Status.SUCCESS) {
				sendButton.setDisabled(false);
			}
		}
	};

	public void onSend() {
		if (uploader.getSuccessUploads() <= 0) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		if (!vm.validate())
			return;

		GUIKeystore keystore=new GUIKeystore();
		if(keystorePanel.getKeystore()!=null)
			keystore=keystorePanel.getKeystore();
		
		keystore.setOrganizationAlias(vm.getValueAsString("localCAalias"));
		keystore.setPassword(vm.getValueAsString("password"));
		keystore.setTenantId(keystorePanel.getTenantId());
		
		SignService.Instance.get().imporKeystore(keystore, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				keystorePanel.init();

				// Cleanup the upload folder
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						destroy();
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});
	}
}