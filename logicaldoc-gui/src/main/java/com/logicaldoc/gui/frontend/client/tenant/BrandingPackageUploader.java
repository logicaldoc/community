package com.logicaldoc.gui.frontend.client.tenant;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBranding;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new branding package
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class BrandingPackageUploader extends Window {

	private IButton uploadButton;

	private Upload uploader;

	private TenantBrandingPanel panel;

	public BrandingPackageUploader(TenantBrandingPanel panel) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadbrandingpackage"));
		setMinWidth(460);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.panel = panel;

		uploadButton = new IButton(I18N.message("upload"));
		uploadButton.addClickHandler(event -> onUpload());

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.setWidth100();

		uploader = new Upload(uploadButton);
		uploader.setFileTypes("*.zip");
		layout.addMember(uploader);
		layout.addMember(uploadButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(
				event -> DocumentService.Instance.get().cleanUploadedFileFolder(new IgnoreAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				}));

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new IgnoreAsyncCallback<>());
	}

	public void onUpload() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		TenantService.Instance.get().importBrandingPackage(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIBranding branding) {
				panel.update(branding);
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<>() {

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
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}