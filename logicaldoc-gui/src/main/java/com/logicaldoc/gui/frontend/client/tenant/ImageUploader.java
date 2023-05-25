package com.logicaldoc.gui.frontend.client.tenant;

import com.google.gwt.dom.client.Style.BorderStyle;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Label;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload new branding images
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class ImageUploader extends Window {

	private IButton uploadButton;

	private Upload uploader;

	private Label customExternalDropZone;

	private String imageName;

	private TenantBrandingPanel panel;

	public ImageUploader(String imageName, TenantBrandingPanel panel) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadnewimage"));
		setMinWidth(460);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.imageName = imageName;
		this.panel = panel;

		customExternalDropZone = new Label();
		customExternalDropZone.setText(I18N.message("dropimagehere"));
		customExternalDropZone.setWidth((getWidth() - 5) + "px");
		customExternalDropZone.setHeight("40px");
		customExternalDropZone.getElement().getStyle().setBorderStyle(BorderStyle.DASHED);
		customExternalDropZone.getElement().getStyle().setBorderWidth(1, Unit.PX);
		customExternalDropZone.getElement().getStyle().setPadding(10, Unit.PX);

		uploadButton = new IButton(I18N.message("upload"));
		uploadButton.addClickHandler(event -> onUpload());

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.setWidth100();

		layout.addMember(customExternalDropZone);

		uploader = new Upload(uploadButton);
		uploader.setFileTypes("*.png");
		layout.addMember(uploader);
		layout.addMember(uploadButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(event -> DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				// Nothing to do0
			}

			@Override
			public void onSuccess(Void result) {
				destroy();
			}
		}));

		addItem(layout);

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

	public void onUpload() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		TenantService.Instance.get().encodeBrandingImage(new AsyncCallback<String>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(String imageContent) {
				panel.updateImage(imageName, imageContent);
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