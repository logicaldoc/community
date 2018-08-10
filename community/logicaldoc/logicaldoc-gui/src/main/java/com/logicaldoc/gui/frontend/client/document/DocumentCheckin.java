package com.logicaldoc.gui.frontend.client.document;

import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.BooleanItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a checked-out document to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentCheckin extends Window {
	private IButton sendButton;

	private MultiUploader multiUploader;

	private ValuesManager vm;

	private GUIDocument document;

	private String fileName;

	public DocumentCheckin(GUIDocument document, String filename) {
		this.document = document;
		this.fileName = filename;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("checkin"));
		setWidth(420);
		setHeight(230);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		BooleanItem versionItem = new BooleanItem();
		versionItem.setName("majorversion");
		versionItem.setTitle(I18N.message("majorversion"));

		final BooleanItem filenameItem = new BooleanItem();
		filenameItem.setName("checkfilename");
		filenameItem.setTitle(I18N.message("checkfilename"));
		filenameItem.setDefaultValue(true);
		filenameItem.setWrapTitle(false);
		filenameItem.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (!filenameItem.getValueAsBoolean())
					sendButton.setDisabled(false);
				multiUploader.reset();
			}
		});

		TextItem commentItem = ItemFactory.newTextItem("comment", "comment", null);
		commentItem.setRequired(true);
		commentItem.setWidth(250);

		form.setItems(versionItem, filenameItem, commentItem);

		// Create a new uploader panel and attach it to the window
		multiUploader = new MultiUploader();
		multiUploader.setStyleName("upload");
		multiUploader.setWidth("300px");
		multiUploader.setHeight("60px");
		multiUploader.setFileInputPrefix("LDOC");
		multiUploader.setMaximumFiles(1);
		multiUploader.addOnFinishUploadHandler(onFinishUploaderHandler);
		multiUploader.reset();

		sendButton = new IButton(I18N.message("send"));
		sendButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSend();
			}
		});
		sendButton.setDisabled(true);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);

		layout.addMember(form);
		layout.addMember(multiUploader);
		layout.addMember(sendButton);

		addItem(layout);
	}

	// Load the image in the document and in the case of success attach it to
	// the viewer
	private IUploader.OnFinishUploaderHandler onFinishUploaderHandler = new IUploader.OnFinishUploaderHandler() {
		public void onFinish(IUploader uploader) {
			if (uploader.getStatus() == Status.SUCCESS) {
				sendButton.setDisabled(false);
			}

			// This check is done because IE8 works differently from Firefox
			String uploadedFilename = uploader.getServerInfo().getFileName();
			if (uploadedFilename.lastIndexOf('/') != -1)
				uploadedFilename = uploadedFilename.substring(uploadedFilename.lastIndexOf('/') + 1);
			if (uploadedFilename.lastIndexOf('\\') != -1)
				uploadedFilename = uploadedFilename.substring(uploadedFilename.lastIndexOf('\\') + 1);

			if ("true".equals(vm.getValueAsString("checkfilename")) && !fileName.equals(uploadedFilename)) {
				sendButton.setDisabled(true);
				SC.warn(I18N.message("nosamefilename"));
			}
		}
	};

	public void onSend() {
		if (multiUploader.getSuccessUploads() <= 0) {
			SC.warn(I18N.message("filerequired"));
			return;
		}
		if (!vm.validate())
			return;

		document.setComment(vm.getValueAsString("comment"));
		UpdateDialog bulk = new UpdateDialog(new long[] { document.getId() }, document, UpdateDialog.CONTEXT_CHECKIN,
				"true".equals(vm.getValueAsString("majorversion")));
		bulk.show();
		destroy();
	}

	public String getLanguage() {
		return vm.getValueAsString("language");
	}

	public boolean getImportZip() {
		return "true".equals(vm.getValueAsString("zip"));
	}
}