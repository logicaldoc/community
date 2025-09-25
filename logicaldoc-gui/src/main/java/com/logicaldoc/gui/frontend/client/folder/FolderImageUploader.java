package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new folder image to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class FolderImageUploader extends Window {
	private IButton save;

	private Upload uploader;

	private GUIFolder folder;

	private ChangedHandler changedHandler;

	public FolderImageUploader(GUIFolder folder, ChangedHandler changedHandler) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadimage"));
		setWidth(430);
		setHeight(180);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		this.folder = folder;
		this.changedHandler = changedHandler;

		save = new IButton(I18N.message("save"));
		save.addClickHandler(click -> onUpload());

		IButton delete = new IButton(I18N.message("ddelete"));
		delete.addClickHandler(click -> onDelete());

		uploader = new Upload(save);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(2);
		buttons.setMembers(save, delete);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.addMember(uploader);
		layout.addMember(buttons);
		addItem(layout);

		addCloseClickHandler(click -> cleanUploads());
	}

	private void cleanUploads() {
		DocumentService.Instance.get().cleanUploadedFileFolder(new IgnoreAsyncCallback<>());
	}

	private void onDelete() {
		folder.setTile(null);
		if (changedHandler != null)
			changedHandler.onChanged(null);
		destroy();
	}

	private void onUpload() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		FolderService.Instance.get().readImage(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				SC.warn(caught.getMessage());
				cleanUploads();
			}

			@Override
			public void onSuccess(String content) {
				folder.setTile(content);
				if (changedHandler != null)
					changedHandler.onChanged(null);
				cleanUploads();
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