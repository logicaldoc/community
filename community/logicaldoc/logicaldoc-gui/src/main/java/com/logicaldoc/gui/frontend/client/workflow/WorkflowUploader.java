package com.logicaldoc.gui.frontend.client.workflow;

import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new workflow schema to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class WorkflowUploader extends Window {

	private MultiUploader uploader;

	private IButton sendButton;

	private VLayout layout = new VLayout();

	private WorkflowDesigner designer;

	public WorkflowUploader(WorkflowDesigner designer) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		this.designer = designer;
		setTitle(I18N.message("uploadworkflow"));
		setWidth(420);
		setHeight(140);
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

		layout.addMember(uploader);
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
		}
	};

	public void onSend() {
		if (uploader.getSuccessUploads() <= 0) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		WorkflowService.Instance.get().importSchema(new AsyncCallback<GUIWorkflow>() {

			@Override
			public void onFailure(Throwable caught) {
				SC.warn(caught.getMessage());
			}

			@Override
			public void onSuccess(GUIWorkflow result) {
				if (result != null) {
					result.setId(designer.getWorkflow().getId());
					result.setName(designer.getWorkflow().getName());
					designer.redraw(result);
					designer.saveModel();

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
			}

		});
	}
}