package com.logicaldoc.gui.frontend.client.metadata.stamp;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new stamp
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampUploader extends Window {

	private IButton submit;

	private Upload uploader;

	private long stampId;

	private StampProperties panel;

	public StampUploader(long stampId, StampProperties panel) {
		this.stampId = stampId;
		this.panel = panel;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadstamp"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		submit = new IButton(I18N.message("submit"));
		submit.addClickHandler(event -> onSubmit());

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);

		uploader = new Upload(submit);
		uploader.setFileTypes("*.png");

		Label spacer = new Label("&nbsp;");
		spacer.setWidth(420);
		spacer.setHeight(5);

		layout.addMember(spacer);
		layout.addMember(uploader);
		layout.addMember(submit);

		addCloseClickHandler(event -> DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void result) {
				destroy();
			}
		}));

		addItem(layout);
	}

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		StampService.Instance.get().saveImage(stampId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg) {
				panel.refreshStampImage();
				close();
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