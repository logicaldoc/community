package com.logicaldoc.gui.frontend.client.tenant;

import gwtupload.client.IFileInput.FileInputType;
import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

import com.google.gwt.dom.client.Style.BorderStyle;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;
import com.logicaldoc.gui.common.client.beans.GUIBranding;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new branding package
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class PackageUploader extends Window {

	private IButton uploadButton;

	private MultiUploader multiUploader;

	private Label customExternalDropZone;

	private TenantBrandingPanel panel;

	public PackageUploader(TenantBrandingPanel panel) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadbrandingpackage"));
		setWidth(460);
		setHeight(200);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.panel = panel;

		customExternalDropZone = new Label();
		customExternalDropZone.setText(I18N.message("dropbrandingpackagehere"));
		customExternalDropZone.setWidth((getWidth() - 5) + "px");
		customExternalDropZone.setHeight("40px");
		customExternalDropZone.getElement().getStyle().setBorderStyle(BorderStyle.DASHED);
		customExternalDropZone.getElement().getStyle().setBorderWidth(1, Unit.PX);
		customExternalDropZone.getElement().getStyle().setPadding(10, Unit.PX);
		multiUploader = new MultiUploader(FileInputType.CUSTOM.with(
				(Widget) new Button(I18N.message("clickmeordropfiles"))).withZone(customExternalDropZone));
		multiUploader.addOnFinishUploadHandler(onFinishUploaderHandler);
		multiUploader.addOnStartUploadHandler(onStartUploaderHandler);
		multiUploader.setFileInputPrefix("BPKG");
		multiUploader.setMaximumFiles(1);
		multiUploader.setValidExtensions(".zip", ".ZIP");
		multiUploader.reset();

		uploadButton = new IButton(I18N.message("upload"));
		uploadButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onUpload();
			}
		});

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.setWidth100();

		layout.addMember(customExternalDropZone);
		layout.addMember(multiUploader);
		layout.addMember(uploadButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
			}

			@Override
			public void onSuccess(Void result) {
			}
		});
	}

	private IUploader.OnFinishUploaderHandler onFinishUploaderHandler = new IUploader.OnFinishUploaderHandler() {
		public void onFinish(IUploader uploader) {
			if (uploader.getStatus() == Status.SUCCESS || multiUploader.getSuccessUploads() > 0) {
				uploadButton.setDisabled(false);
			}
		}
	};

	private IUploader.OnStartUploaderHandler onStartUploaderHandler = new IUploader.OnStartUploaderHandler() {
		public void onStart(IUploader uploader) {
			uploadButton.setDisabled(true);
		}
	};

	public void onUpload() {
		TenantService.Instance.get().importBrandingPackage(new AsyncCallback<GUIBranding>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBranding branding) {
				panel.update(branding);
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						destroy();
					}

					@Override
					public void onSuccess(Void arg) {
						destroy();
					}
				});
			}
		});
	}
}