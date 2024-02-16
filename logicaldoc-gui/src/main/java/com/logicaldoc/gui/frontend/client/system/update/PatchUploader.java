package com.logicaldoc.gui.frontend.client.system.update;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.UpdateService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new patch package to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class PatchUploader extends Window {
	private IButton submitButton;

	private Upload uploader;

	private PatchPanel panel;

	public PatchUploader(PatchPanel panel) {
		this.panel=panel;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadpatch"));
		setWidth(430);
		setHeight(180);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		submitButton = new IButton(I18N.message("upload"));
		submitButton.addClickHandler(event -> onSubmit());

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);

		uploader = new Upload(submitButton);
		layout.addMember(uploader);
		layout.addMember(submitButton);

		addItem(layout);

		addCloseClickHandler(event -> cleanUploads());
	}

	private void cleanUploads() {
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

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		UpdateService.Instance.get().loadPatch(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				SC.warn(caught.getMessage());
				cleanUploads();
			}

			@Override
			public void onSuccess(String result) {
				try {
				if (result == null || "".equals(result)) {
					panel.showList();
					destroy();
				} else {
					SC.warn(I18N.message(result));
				}
				}finally {
					cleanUploads();
				}
			}

		});
	}
}