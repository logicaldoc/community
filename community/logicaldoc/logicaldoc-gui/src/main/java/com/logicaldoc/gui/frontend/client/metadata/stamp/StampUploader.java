package com.logicaldoc.gui.frontend.client.metadata.stamp;

import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new stamp
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampUploader extends Window {

	private IButton saveButton;

	private MultiUploader multiUploader;

	private long stampId;

	private StampDetailsPanel panel;

	public StampUploader(long stampId, StampDetailsPanel panel) {
		this.stampId = stampId;
		this.panel = panel;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadstamp"));
		setWidth(460);
		setHeight(150);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		// Create a new uploader panel and attach it to the window
		multiUploader = new MultiUploader();
		multiUploader.addOnStartUploadHandler(onStartUploaderHandler);
		multiUploader.addOnFinishUploadHandler(onFinishUploaderHandler);
		multiUploader.setStyleName("upload");
		multiUploader.setWidth("400px");
		multiUploader.setHeight("50px");
		multiUploader.setFileInputPrefix("LDOC");
		multiUploader.reset();
		multiUploader.setMaximumFiles(1);
		multiUploader.setValidExtensions("png", "PNG");

		saveButton = new IButton(I18N.message("save"));
		saveButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);

		layout.addMember(multiUploader);
		layout.addMember(saveButton);

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});

		addItem(layout);
	}

	private IUploader.OnFinishUploaderHandler onFinishUploaderHandler = new IUploader.OnFinishUploaderHandler() {
		public void onFinish(IUploader uploader) {
			if (uploader.getStatus() == Status.SUCCESS && multiUploader.getSuccessUploads() > 0)
				saveButton.setDisabled(false);
		}
	};

	private IUploader.OnStartUploaderHandler onStartUploaderHandler = new IUploader.OnStartUploaderHandler() {
		public void onStart(IUploader uploader) {
			saveButton.setDisabled(true);
		}
	};

	public void onSave() {
		if (multiUploader.getSuccessUploads() <= 0) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		StampService.Instance.get().saveImage(stampId, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
				close();
			}

			@Override
			public void onSuccess(Void arg) {
				panel.refreshStampImage();
				close();
			}
		});
	}
}