package com.logicaldoc.gui.frontend.client.metadata.form;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.menu.QuickSearchTray;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays the header image
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class FormImageTile extends HLayout {

	private GUIForm form;

	private ChangedHandler changedHandler;

	public FormImageTile(GUIForm form, ChangedHandler changedHandler) {
		this.form = form;
		this.changedHandler = changedHandler;

		setMembersMargin(1);
		setAlign(Alignment.LEFT);
		setOverflow(Overflow.HIDDEN);

		initGUI();
	}

	private void initGUI() {
		Canvas[] members = getMembers();
		if (members != null && members.length > 0)
			for (Canvas canvas : members)
				removeChild(canvas);

		IButton upload = new IButton(I18N.message("uploadheader"));
		upload.addClickHandler(event -> new Uploader(form).show());
		upload.setDisabled(form.getId() == 0L);

		IButton clear = new IButton(I18N.message("clear"));
		clear.addClickHandler(event -> {
			form.setHeaderImage(null);
			changedHandler.onChanged(null);
			initGUI();
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(2);
		buttons.setAutoHeight();
		buttons.setMembers(upload, clear);

		HTMLFlow flow = null;
		if (form.getHeaderImage() != null)
			flow = new HTMLFlow(
					"<img src='" + form.getHeaderImage() + "' style='float:body; max-width:300px' align='body' />");
		else
			flow = new HTMLFlow(" ");

		VLayout image = new VLayout();
		image.setAlign(VerticalAlignment.TOP);
		image.setMargin(1);
		image.setMembersMargin(2);
		image.setMembers(buttons, flow);

		setMembers(image);
	}

	private class Uploader extends Window {

		private IButton saveButton;

		private Upload upload;

		private GUIForm form;

		public Uploader(GUIForm form) {
			this.form = form;

			setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
			setTitle(I18N.message("uploadheaderimage"));
			setWidth(460);
			setHeight(150);
			setCanDragResize(true);
			setIsModal(true);
			setShowModalMask(true);
			centerInPage();

			saveButton = new IButton(I18N.message("save"));
			saveButton.addClickHandler(event -> onSave());

			VLayout layout = new VLayout();
			layout.setMembersMargin(5);
			layout.setMargin(2);

			upload = new Upload(saveButton);
			upload.setFileTypes("*.png,*.jpg,*.jpeg,*.gif");
			layout.addMember(upload);
			layout.addMember(saveButton);

			addCloseClickHandler(
					event -> DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void result) {
							destroy();
						}
					}));

			addItem(layout);
		}

		public void onSave() {
			if (upload.getUploadedFile() == null) {
				SC.warn(I18N.message("filerequired"));
				return;
			}

			FormService.Instance.get().processImage(new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(String imageSrc) {
					form.setHeaderImage(imageSrc);
					FormImageTile.this.initGUI();
					DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void result) {
							destroy();
							close();
							if (changedHandler != null)
								changedHandler.onChanged(null);
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

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof QuickSearchTray)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
