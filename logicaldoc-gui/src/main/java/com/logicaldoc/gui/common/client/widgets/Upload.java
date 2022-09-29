package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.List;

import org.wisepersist.gwt.uploader.client.Uploader;
import org.wisepersist.gwt.uploader.client.events.FileDialogCompleteEvent;
import org.wisepersist.gwt.uploader.client.events.FileDialogCompleteHandler;
import org.wisepersist.gwt.uploader.client.events.FileQueueErrorEvent;
import org.wisepersist.gwt.uploader.client.events.FileQueueErrorHandler;
import org.wisepersist.gwt.uploader.client.events.UploadErrorEvent;
import org.wisepersist.gwt.uploader.client.events.UploadErrorHandler;
import org.wisepersist.gwt.uploader.client.events.UploadProgressEvent;
import org.wisepersist.gwt.uploader.client.events.UploadProgressHandler;
import org.wisepersist.gwt.uploader.client.events.UploadSuccessEvent;
import org.wisepersist.gwt.uploader.client.events.UploadSuccessHandler;

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.ui.HorizontalPanel;
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

	private List<String> uploadedFiles = new ArrayList<String>();

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
				.setUploadProgressHandler(new UploadProgressHandler() {
					public boolean onUploadProgress(UploadProgressEvent uploadProgressEvent) {
						progressLabel.setText(NumberFormat.getPercentFormat()
								.format(uploadProgressEvent.getBytesComplete() / uploadProgressEvent.getBytesTotal()));
						return true;
					}
				}).setUploadSuccessHandler(new UploadSuccessHandler() {
					public boolean onUploadSuccess(UploadSuccessEvent uploadSuccessEvent) {
						resetText();
						progressLabel.setText(uploadSuccessEvent.getFile().getName());

						if (confirmButton != null)
							confirmButton.setDisabled(false);
						if (submitButton != null)
							submitButton.setDisabled(false);
						
						return true;
					}
				}).setFileDialogCompleteHandler(new FileDialogCompleteHandler() {
					public boolean onFileDialogComplete(FileDialogCompleteEvent fileDialogCompleteEvent) {
						if (fileDialogCompleteEvent.getTotalFilesInQueue() > 0
								&& uploader.getStats().getUploadsInProgress() <= 0) {
							progressLabel.setText("0%");
							uploader.startUpload();
						}
						return true;
					}
				}).setFileQueueErrorHandler(new FileQueueErrorHandler() {
					public boolean onFileQueueError(FileQueueErrorEvent fileQueueErrorEvent) {
						resetText();
						SC.warn(I18N.message("uploadoffile") + " " + fileQueueErrorEvent.getFile().getName() + " "
								+ I18N.message("failedueto") + " [" + fileQueueErrorEvent.getErrorCode().toString()
								+ "]: " + fileQueueErrorEvent.getMessage());
						return true;
					}
				}).setUploadErrorHandler(new UploadErrorHandler() {
					public boolean onUploadError(UploadErrorEvent uploadErrorEvent) {
						resetText();
						SC.warn(I18N.message("uploadoffile") + " " + uploadErrorEvent.getFile().getName() + " "
								+ I18N.message("failedueto") + " [" + uploadErrorEvent.getErrorCode().toString() + "]: "
								+ uploadErrorEvent.getMessage());
						return true;
					}
				});

		add(uploader);
		add(progressLabel);
		setCellHorizontalAlignment(uploader, HorizontalPanel.ALIGN_LEFT);
		setCellHorizontalAlignment(progressLabel, HorizontalPanel.ALIGN_LEFT);
	}

	private void resetText() {
		progressLabel.setText("");
	}

	public void setFileTypes(String fileTypes) {
		uploader.setFileTypes(fileTypes);
	}

	public List<String> getUploadedFiles() {
		return uploadedFiles;
	}
}