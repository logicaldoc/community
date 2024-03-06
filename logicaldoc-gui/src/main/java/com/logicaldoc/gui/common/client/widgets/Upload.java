package com.logicaldoc.gui.common.client.widgets;

import org.wisepersist.gwt.uploader.client.Uploader;

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.fields.SubmitItem;

/**
 * A widget to upload a single file to the platform
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class Upload extends VerticalPanel {

	private Uploader uploader = new Uploader();

	private final Label progressLabel = new Label();

	private String uploadedFile;

	public Upload(SubmitItem submitButton) {
		this(null, submitButton);
	}

	public Upload(IButton confirmButton) {
		this(confirmButton, null);
	}

	public Upload(IButton confirmButton, SubmitItem submitButton) {
		progressLabel.setStyleName("progressLabel");
		uploader.setUploadURL(Util.contextPath() + "upload").setButtonText(I18N.message("clicktoupload"))
				.setButtonHeight(22).setFileSizeLimit(Session.get().getConfig("upload.maxsize") + " MB")
				.setButtonCursor(Uploader.Cursor.HAND).setButtonAction(Uploader.ButtonAction.SELECT_FILE)
				.setUploadProgressHandler(uploadProgressEvent -> {
					progressLabel.setText(NumberFormat.getPercentFormat().format(
							uploadProgressEvent.getBytesComplete() / (double) uploadProgressEvent.getBytesTotal()));
					return true;
				}).setUploadSuccessHandler(uploadSuccessEvent -> {
					resetText();
					String fileName = uploadSuccessEvent.getFile().getName();
					String fileExtension = fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase();

					if (!Util.isAllowedForUpload(fileName)) {
						uploader.cancelUpload(uploadSuccessEvent.getFile().getId(), false);
						fileName = null;
						SC.warn(I18N.message("disallowedext", fileExtension));
						return false;
					}

					progressLabel.setText(fileName);
					uploadedFile = fileName;

					if (confirmButton != null)
						confirmButton.setDisabled(false);
					if (submitButton != null)
						submitButton.setDisabled(false);

					return true;
				}).setFileDialogCompleteHandler(fileDialogCompleteEvent -> {
					if (fileDialogCompleteEvent.getTotalFilesInQueue() > 0
							&& uploader.getStats().getUploadsInProgress() <= 0) {
						progressLabel.setText("0%");
						uploader.startUpload();
					}
					return true;
				}).setFileQueueErrorHandler(fileQueueErrorEvent -> {
					resetText();
					SC.warn(I18N.message("uploadoffile") + " " + fileQueueErrorEvent.getFile().getName() + " "
							+ I18N.message("failedueto") + " [" + fileQueueErrorEvent.getErrorCode().toString() + "]: "
							+ fileQueueErrorEvent.getMessage());
					uploadedFile = null;
					return true;
				}).setUploadErrorHandler(uploadErrorEvent -> {
					resetText();
					SC.warn(I18N.message("uploadoffile") + " " + uploadErrorEvent.getFile().getName() + " "
							+ I18N.message("failedueto") + " [" + uploadErrorEvent.getErrorCode().toString() + "]: "
							+ uploadErrorEvent.getMessage());
					uploadedFile = null;
					return true;
				});
		add(uploader);
		add(progressLabel);
		setCellHorizontalAlignment(uploader, HasHorizontalAlignment.ALIGN_LEFT);
		setCellHorizontalAlignment(progressLabel, HasHorizontalAlignment.ALIGN_LEFT);
	}

	private void resetText() {
		progressLabel.setText("");
	}

	public void setFileTypes(String fileTypes) {
		uploader.setFileTypes(fileTypes);
	}

	public String getUploadedFile() {
		return uploadedFile;
	}
}