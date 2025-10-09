package com.logicaldoc.gui.frontend.client.security.saml;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload SAML resources to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class SamlUploader extends Window {

	private IButton submit;

	private Upload uploader;

	private FormItem srcItem;

	public SamlUploader(String title, FormItem srcItem) {
		this.srcItem = srcItem;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(title));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMinWidth(450);
		setAutoSize(true);

		submit = new IButton(I18N.message("submit"));
		submit.addClickHandler(event -> onSubmit());
		submit.setDisabled(true);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		uploader = new Upload(submit);
		uploader.setFileTypes("*" + srcItem.getName().substring(srcItem.getName().lastIndexOf('.')));

		layout.addMember(uploader);
		layout.addMember(submit);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(
				event -> DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						destroy();
					}
				}));

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new IgnoreAsyncCallback<>());
	}

	public void onSubmit() {
		SamlService.Instance.get().importResource(srcItem.getName(), new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(String content) {
				srcItem.setValue(content);
				destroy();
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