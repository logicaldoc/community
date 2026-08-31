package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.wisepersist.gwt.uploader.client.Uploader;
import org.wisepersist.gwt.uploader.client.events.FileQueuedEvent;
import org.wisepersist.gwt.uploader.client.progress.ProgressBar;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.HasVerticalAlignment;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;

/**
 * A widget to upload multiple files to the platform
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class MultipleUpload extends HorizontalPanel {

	private static final String DROP_FILES_LABEL_HOVER = "dropFilesLabelHover";

	private Uploader uploader = new Uploader();

	private List<String> uploadedFiles = new ArrayList<>();

	public MultipleUpload(IButton confirmButton) {
		this(confirmButton, "dropfileshere");
	}

	public MultipleUpload(IButton confirmButton, String label) {
		final VerticalPanel progressBarPanel = new VerticalPanel();
		final Map<String, ProgressBar> progressBars = new LinkedHashMap<>();
		final Map<String, Image> cancelButtons = new LinkedHashMap<>();
		prepareUploader(confirmButton, progressBarPanel, progressBars, cancelButtons);

		VerticalPanel verticalPanel = new VerticalPanel();
		verticalPanel.add(uploader);

		if (Uploader.isAjaxUploadWithProgressEventsSupported()) {
			final Label dropFilesLabel = new Label(I18N.message(label));
			dropFilesLabel.setStyleName("dropFilesLabel");
			dropFilesLabel.addDragOverHandler(event -> {
				if (!uploader.getButtonDisabled()) {
					dropFilesLabel.addStyleName(DROP_FILES_LABEL_HOVER);
				}
			});
			dropFilesLabel.addDragLeaveHandler(event -> dropFilesLabel.removeStyleName(DROP_FILES_LABEL_HOVER));
			dropFilesLabel.addDropHandler(event -> {
				dropFilesLabel.removeStyleName(DROP_FILES_LABEL_HOVER);

				if (uploader.getStats().getUploadsInProgress() <= 0) {
					progressBarPanel.clear();
					progressBars.clear();
					cancelButtons.clear();
				}

				uploader.addFilesToQueue(Uploader.getDroppedFiles(event.getNativeEvent()));
				event.preventDefault();
			});
			verticalPanel.add(dropFilesLabel);
		}

		add(verticalPanel);
		add(progressBarPanel);
		setVerticalAlignment(HasVerticalAlignment.ALIGN_MIDDLE);
		setCellHorizontalAlignment(uploader, HasHorizontalAlignment.ALIGN_LEFT);
		setCellHorizontalAlignment(progressBarPanel, HasHorizontalAlignment.ALIGN_RIGHT);
	}

	private void prepareUploader(IButton confirmButton, final VerticalPanel progressBarPanel,
			final Map<String, ProgressBar> progressBars, final Map<String, Image> cancelButtons) {
		uploader.setUploadURL(Util.contextPath() + "upload").setButtonText(I18N.message("clicktoupload"))
				.setButtonHeight(22).setFileSizeLimit(Session.get().getConfig("upload.maxsize") + " MB")
				.setButtonCursor(Uploader.Cursor.HAND).setButtonAction(Uploader.ButtonAction.SELECT_FILES)
				.setFileQueuedHandler(fileQueuedEvent -> handleQueueEvent(progressBarPanel, progressBars, cancelButtons,
						fileQueuedEvent))
				.setUploadProgressHandler(uploadProgressEvent -> {
					ProgressBar progressBar = progressBars.get(uploadProgressEvent.getFile().getId());
					progressBar.setProgress(
							(double) uploadProgressEvent.getBytesComplete() / uploadProgressEvent.getBytesTotal());
					return true;
				}).setUploadCompleteHandler(uploadCompleteEvent -> {
					if (!uploadedFiles.contains(uploadCompleteEvent.getFile().getName()))
						uploadedFiles.add(uploadCompleteEvent.getFile().getName());
					cancelButtons.get(uploadCompleteEvent.getFile().getId()).removeFromParent();
					uploader.startUpload();
					return true;
				}).setUploadSuccessHandler(uploadSuccessEvent -> {
					if (confirmButton != null)
						confirmButton.setDisabled(false);
					return false;
				}).setFileDialogStartHandler(fileDialogStartEvent -> {
					if (uploader.getStats().getUploadsInProgress() <= 0) {
						// Clear the uploads that have completed, if none
						// are in process
						progressBarPanel.clear();
						progressBars.clear();
						cancelButtons.clear();
					}
					return true;
				}).setFileDialogCompleteHandler(fileDialogCompleteEvent -> {
					if (fileDialogCompleteEvent.getTotalFilesInQueue() > 0
							&& uploader.getStats().getUploadsInProgress() <= 0) {
						uploader.startUpload();
					}
					return true;
				}).setFileQueueErrorHandler(fileQueueErrorEvent -> {
					if (!uploadedFiles.contains(fileQueueErrorEvent.getFile().getName()))
						uploadedFiles.add(fileQueueErrorEvent.getFile().getName());
					SC.warn(I18N.message("uploadoffile") + " " + fileQueueErrorEvent.getFile().getName() + " "
							+ I18N.message("failedueto") + " [" + fileQueueErrorEvent.getErrorCode().toString() + "]: "
							+ fileQueueErrorEvent.getMessage());
					return true;
				}).setUploadErrorHandler(uploadErrorEvent -> {
					if (!uploadedFiles.contains(uploadErrorEvent.getFile().getName()))
						uploadedFiles.add(uploadErrorEvent.getFile().getName());
					cancelButtons.get(uploadErrorEvent.getFile().getId()).removeFromParent();
					SC.warn(I18N.message("uploadoffile") + " " + uploadErrorEvent.getFile().getName() + " "
							+ I18N.message("failedueto") + " [" + uploadErrorEvent.getErrorCode().toString() + "]: "
							+ uploadErrorEvent.getMessage());
					return true;
				});
	}

	private boolean handleQueueEvent(final VerticalPanel progressBarPanel, final Map<String, ProgressBar> progressBars,
			final Map<String, Image> cancelButtons, final FileQueuedEvent fileQueuedEvent) {
		// Create a Progress Bar for this file
		final ProgressBar progressBar = new ProgressBar(0.0, 1.0, 0.0, new CancelProgressBarTextFormatter());
		progressBar.setTitle(fileQueuedEvent.getFile().getName());
		progressBar.setHeight("18px");
		progressBar.setWidth("200px");
		progressBars.put(fileQueuedEvent.getFile().getId(), progressBar);

		// Add Cancel Button Image
		final Image cancelButton = new Image(Util.imageUrl("cancel.png"));
		cancelButton.setStyleName("cancelButton");
		cancelButton.addClickHandler((ClickEvent event) -> {
			uploader.cancelUpload(fileQueuedEvent.getFile().getId(), false);
			progressBars.get(fileQueuedEvent.getFile().getId()).setProgress(-1.0d);
			cancelButton.removeFromParent();
		});
		cancelButtons.put(fileQueuedEvent.getFile().getId(), cancelButton);

		// Add the Bar and Button to the interface
		HorizontalPanel progressBarAndButtonPanel = new HorizontalPanel();
		progressBarAndButtonPanel.add(progressBar);
		progressBarAndButtonPanel.add(cancelButton);
		progressBarPanel.add(progressBarAndButtonPanel);

		String fileName = fileQueuedEvent.getFile().getName();
		String fileExtension = fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase();

		if (!Util.isAllowedForUpload(fileName)) {
			uploader.cancelUpload(fileQueuedEvent.getFile().getId(), false);
			uploadedFiles.remove(fileName);
			SC.warn(I18N.message("disallowedext", fileExtension));
			return false;
		}

		return true;
	}

	public void setFileTypes(String fileTypes) {
		uploader.setFileTypes(fileTypes);
	}

	public void setMaxUploads(int maxUploads) {
		uploader.setFileUploadLimit(maxUploads);
	}

	protected class CancelProgressBarTextFormatter extends ProgressBar.TextFormatter {
		@Override
		protected String getText(ProgressBar bar, double curProgress) {
			if (curProgress < 0) {
				return I18N.message("cancelled");
			}
			return ((int) (100 * bar.getPercent())) + "%";
		}
	}

	public List<String> getUploadedFiles() {
		return uploadedFiles;
	}
}