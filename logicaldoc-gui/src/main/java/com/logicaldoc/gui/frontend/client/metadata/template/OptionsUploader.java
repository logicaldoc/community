package com.logicaldoc.gui.frontend.client.metadata.template;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

/**
 * This popup window is used to upload a new options file to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class OptionsUploader extends Window {

	private SubmitItem sendButton;

	private MultiUploader uploader;

	private ValuesManager vm;

	private DynamicForm form;

	private Options options;

	public OptionsUploader(Options options) {
		this.options = options;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadoptions"));
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
		sendButton.setTitle(I18N.message("send"));
		sendButton.setDisabled(true);
		sendButton.setAlign(Alignment.RIGHT);
		sendButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSend();
			}
		});

		form.setItems(sendButton);

		// Create a new uploader panel and attach it to the window
		uploader = new MultiUploader();
		uploader.setMaximumFiles(1);
		uploader.setStyleName("upload");
		uploader.setFileInputPrefix("LDOC_OPT");
		uploader.setHeight("30px");
		uploader.reset();

		// Add a finish handler which will load the image once the upload
		// finishes
		uploader.addOnFinishUploadHandler(onFinishUploaderHandler);

		addItem(uploader);
		addItem(form);
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

		LD.contactingServer();
		AttributeSetService.Instance.get().parseOptions(options.getSetId(), options.getAttribute(), new AsyncCallback<String[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				options.refresh();
				LD.clearPrompt();
			}

			@Override
			public void onSuccess(String[] ret) {
				options.refresh();
				LD.clearPrompt();
				destroy();
			}
		});
	}
}