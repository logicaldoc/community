package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

/**
 * This popup window is used to upload a new version's file.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class ReplaceVersionFile extends Window {
	private IButton saveButton;

	private MultiUploader multiUploader;

	private ValuesManager vm;

	private GUIDocument document;

	private String fileVersion;

	public ReplaceVersionFile(GUIDocument document, String fileVersion) {
		this.document = document;
		this.fileVersion = fileVersion;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("replacefile"));
		setWidth(420);
		
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		TextItem commentItem = ItemFactory.newTextItem("comment", "comment", null);
		commentItem.setRequired(true);
		commentItem.setWidth(250);

		form.setItems(commentItem);

		// Create a new uploader panel and attach it to the window
		multiUploader = new MultiUploader();
		multiUploader.setStyleName("upload");
		multiUploader.setWidth("300px");
		multiUploader.setHeight("60px");
		multiUploader.setFileInputPrefix("LDOC");
		multiUploader.setMaximumFiles(1);
		multiUploader.addOnFinishUploadHandler(onFinishUploaderHandler);
		multiUploader.reset();

		saveButton = new IButton(I18N.message("save"));
		saveButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});
		saveButton.setDisabled(true);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);

		layout.addMember(form);
		layout.addMember(multiUploader);
		layout.addMember(saveButton);

		addItem(layout);
	}

	// Load the image in the document and in the case of success attach it to
	// the viewer
	private IUploader.OnFinishUploaderHandler onFinishUploaderHandler = new IUploader.OnFinishUploaderHandler() {
		public void onFinish(IUploader uploader) {
			if (uploader.getStatus() == Status.SUCCESS) {
				saveButton.setDisabled(false);
			}
		}
	};

	public void onSave() {
		if (!vm.validate())
			return;

		if (multiUploader.getSuccessUploads() <= 0) {
			SC.warn(I18N.message("filerequired"));
			return;
		}
		
		DocumentService.Instance.get().replaceFile(document.getId(), fileVersion, vm.getValueAsString("comment"),
				new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						destroy();
					}
				});
	}

	public String getLanguage() {
		return vm.getValueAsString("language");
	}

	public boolean getImportZip() {
		return "true".equals(vm.getValueAsString("zip"));
	}
}