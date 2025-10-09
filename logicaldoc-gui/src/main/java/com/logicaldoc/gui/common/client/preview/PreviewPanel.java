package com.logicaldoc.gui.common.client.preview;

import java.util.List;

import com.google.gwt.http.client.URL;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;
import com.logicaldoc.gui.common.client.controllers.ReadingRequestController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.MessageLabel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.ReadingRequestService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel is used to show the document preview.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
public class PreviewPanel extends VLayout {

	protected HTMLPane preview = null;

	protected HTMLFlow media = null;

	protected HTMLFlow html = null;

	protected HTMLFlow dicom = null;

	protected MailPreviewPanel mail = null;

	protected Canvas reload = null;

	protected Label disabled = null;

	protected long docId;

	protected boolean accessGranted = false;

	protected GUIDocument document;

	protected int width;

	protected int height;

	protected boolean redrawing = false;

	protected ToolStrip topConfirmReadingToolStrip = null;

	protected ToolStripButton topConfirmReadingButton = new ToolStripButton(I18N.message("confirmreading"));

	protected ToolStrip bottomConfirmReadingToolStrip = null;

	protected ToolStripButton bottomConfirmReadingButton = new ToolStripButton(I18N.message("confirmreading"));

	protected Label topConfirmReadingLabel;

	protected Label bottomConfirmReadingLabel;

	public PreviewPanel(final GUIDocument document) {
		this.document = document;
		this.docId = document.getId();

		declareOnReadingCompleted(this);
	}

	@Override
	protected void onDraw() {
		if (!isPreviewAvailable()) {
			showDisabledPanel();
			return;
		}

		DocumentProtectionManager.askForPassword(this.document, doc -> onAccessGranted(document));

		addResizedHandler(event -> doResize());
	}

	private void confirmReading() {
		topConfirmReadingToolStrip.removeMember(topConfirmReadingButton);
		bottomConfirmReadingToolStrip.removeMember(bottomConfirmReadingButton);

		ReadingRequestService.Instance.get().confirmReadings(
				ReadingRequestController.get().getUnconfirmedReadingIds(document.getId()), document.getVersion(),
				new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(Void v) {
						ReadingRequestController.get().confirmReading(document.getId());
						topConfirmReadingLabel.setContents(I18N.message("readingconfirmthanks"));
						bottomConfirmReadingLabel.setContents(I18N.message("readingconfirmthanks"));
					}
				});
	}

	protected void onAccessGranted(final GUIDocument document) {
		accessGranted = true;

		if (Util.isMediaFile(document.getFileName().toLowerCase())) {
			reloadMedia();
		} else if (Util.isWebContentFile(document.getFileName().toLowerCase())) {
			reloadHTML();
		} else if (Util.isEmailFile(document.getFileName().toLowerCase())) {
			reloadMail();
		} else if (Util.isDICOMFile(document.getFileName().toLowerCase())) {
			DocumentService.Instance.get().getById(document.getId(), new DefaultAsyncCallback<>() {

				@Override
				public void handleSuccess(GUIDocument doc) {
					if (doc.isDownload())
						reloadDICOM();
					else
						reloadPreview();
				}
			});
		} else {
			reloadPreview();
		}

		redraw();
	}

	protected boolean isPreviewAvailable() {
		return Menu.enabled(Menu.PREVIEW);
	}

	protected void doResize() {
		if (getWidth() < 20L) {
			// The panel has been closed
			clearContent();
			width = 0;
			height = 0;
			html = null;
			preview = null;
			dicom = null;
			media = null;
			mail = null;
			reload = null;
		} else if (!redrawing
				&& (!isWithinTolerance(width, getWidth(), 20) || !isWithinTolerance(height, getHeight(), 20))) {
			width = getWidth();
			height = getHeight();
			clearContent();
			showReloadPanel();
		}
	}

	private boolean isWithinTolerance(int value, int targetValue, int tolerance) {
		return value >= targetValue - tolerance && value <= targetValue + tolerance;
	}

	@Override
	public synchronized void redraw() {
		redrawing = true;
		try {
			clearContent();
			width = getWidth();
			height = getHeight();
			if (!isPreviewAvailable()) {
				showDisabledPanel();
				return;
			}

			if (accessGranted && getWidth() > 10) {
				if (preview != null) {
					reloadPreview();
				} else if (html != null) {
					reloadHTML();
				} else if (dicom != null) {
					reloadDICOM();
				} else if (media != null) {
					reloadMedia();
				} else if (mail != null) {
					reloadMail();
				}
			}
		} finally {
			redrawing = false;
		}
	}

	/**
	 * Reloads a mail preview
	 */
	protected void reloadMail() {
		DocumentService.Instance.get().extractEmail(docId, document.getFileVersion(), new DefaultAsyncCallback<>() {

			@Override
			public void handleSuccess(final GUIEmail email) {
				if (mail != null)
					removeMember(mail);
				mail = new MailPreviewPanel(email, document, getWidth());
				addMember(mail);
			}
		});
	}

	/**
	 * Reloads a media preview
	 */
	protected void reloadMedia() {
		if (media != null)
			removeMember(media);

		media = new HTMLFlow();
		String contents = "";

		try {
			String url = mediaUrl();

			if (Util.isAudioFile(document.getFileName()))
				contents = Util.audioHTML(url);
			else
				contents = Util.videoHTML(url, getWidth() != null ? "" + (getWidth() - 2) : "",
						getHeight() != null ? "" + (getHeight() - 1) : "");
		} catch (Exception t) {
			GuiLog.info(t.getMessage(), null);
		}

		media.setContents(contents);
		addMember(media);
	}

	/**
	 * Reloads a preview for HTML documents.
	 */
	protected void reloadHTML() {
		if (html != null)
			removeMember(html);

		html = new HTMLPane();
		html.setShowEdges(false);
		html.setContentsURL(htmlUrl());
		html.setContentsType(ContentsType.FRAGMENT);

		addMember(html);
	}

	/**
	 * Reloads a preview for DICOM documents.
	 */
	protected void reloadDICOM() {
		if (dicom != null)
			removeMember(dicom);

		dicom = new HTMLFlow();
		String url = dicomUrl();
		dicom.setContents(
				"<iframe src='" + url + "' style='border:0px solid white; width:" + (getWidth() - 1) + "px; height:"
						+ (getHeight() - 1) + "px; overflow:hidden;' scrolling='no' seamless='seamless'></iframe>");
		addMember(dicom);
	}

	/**
	 * Reloads a preview.
	 */
	protected void reloadPreview() {
		if (preview != null)
			removeMember(preview);
		if (topConfirmReadingToolStrip != null)
			removeMember(topConfirmReadingToolStrip);
		if (bottomConfirmReadingToolStrip != null)
			removeMember(bottomConfirmReadingToolStrip);

		preview = new HTMLPane();

		long maxFileSize = Session.get().getConfigAsLong("gui.preview.maxfilesize") * 1024 * 1024;
		if (maxFileSize > 0 && maxFileSize < document.getFileSize()) {
			preview.setContentsType(ContentsType.FRAGMENT);
			preview.setContents(
					"<div><br><center><b>" + I18N.message("doctoobigtoberendered") + "</b></center></br></div>");
			addMember(preview);
		} else {
			preview.setContentsType(ContentsType.PAGE);
			preview.setContentsURL(generalPreviewUrl());

			if (Feature.enabled(Feature.READING_CONFIRMATION)
					&& ReadingRequestController.get().isReadingConfirmRequired(document.getId())) {
				showTopConfirmReadingToolbar();
			}

			addMember(preview);

			if (Feature.enabled(Feature.READING_CONFIRMATION)
					&& ReadingRequestController.get().isReadingConfirmRequired(document.getId())) {
				showBottomConfirmReadingToolbar();
			}
		}
	}

	protected String generalPreviewUrl() {
		String locale = Session.get().getUser().getLanguage();
		return Util.contextPath() + "prev/index.jsp?docId=" + docId
				+ (document.getFileVersion() != null ? "&fileVersion=" + document.getFileVersion() : "")
				+ "&control=preview&locale=" + locale + "#locale=" + locale.replace('_', '-');
	}

	protected String dicomUrl() {
		return Util.contextPath() + "dicom/index.jsp?input=" + URL.encodeQueryString(
				Util.downloadURL(docId, document.getFileVersion()) + "&sid=" + Session.get().getSid());
	}

	protected String htmlUrl() {
		return Util.downloadURL(docId, document.getFileVersion(), "safe.html", false);
	}

	protected String mediaUrl() {
		return Util.downloadURL(docId, document.getFileVersion(), false);
	}

	protected void clearContent() {
		if (reload != null) {
			removeMember(reload);
		}
		if (preview != null) {
			removeMember(preview);
		}
		if (html != null) {
			removeMember(html);
		}
		if (dicom != null) {
			removeMember(dicom);
		}
		if (media != null) {
			removeMember(media);
		}
		if (mail != null) {
			removeMember(mail);
		}
		if (disabled != null) {
			removeMember(disabled);
		}
	}

	private void showReloadPanel() {
		IButton reloadButton = new IButton(I18N.message("reload"));
		reloadButton.setAutoFit(true);
		reloadButton.setSnapTo("C");
		reloadButton.addClickHandler(event -> redraw());

		reload = new Canvas();
		reload.setWidth100();
		reload.setHeight100();
		reload.setAlign(Alignment.CENTER);
		reload.addChild(reloadButton);

		addMember(reload);
	}

	/**
	 * Displays the label when the preview menu is not enabled
	 */
	private void showDisabledPanel() {
		disabled = new MessageLabel(
				new GUIMessage("<b>" + I18N.message("previewdisabled") + "</b>", GUIMessage.PRIO_WARN), false);
		disabled.setHeight(30);
		disabled.setPadding(10);
		disabled.setAlign(Alignment.CENTER);
		disabled.setValign(VerticalAlignment.CENTER);
		disabled.setShowEdges(false);
		addMember(disabled);
	}

	public void onReadingCompleted() {
		if (Feature.enabled(Feature.READING_CONFIRMATION)) {
			topConfirmReadingButton.setDisabled(false);
			topConfirmReadingButton.setTooltip("");
			topConfirmReadingButton.setStyleName("confirmbutton");
			bottomConfirmReadingButton.setDisabled(false);
			bottomConfirmReadingButton.setTooltip("");
			bottomConfirmReadingButton.setStyleName("confirmbutton");
		}
	}

	private void showTopConfirmReadingToolbar() {
		topConfirmReadingButton.addClickHandler(event -> confirmReading());
		topConfirmReadingButton.setDisabled(true);
		topConfirmReadingButton.setTooltip(I18N.message("readalldoctoenablebutton"));

		topConfirmReadingLabel = prepareConfirmReadingLabel();

		topConfirmReadingToolStrip = new ToolStrip();
		topConfirmReadingToolStrip.setWidth100();
		topConfirmReadingToolStrip.setAlign(Alignment.RIGHT);
		topConfirmReadingToolStrip.addMember(topConfirmReadingLabel);
		topConfirmReadingToolStrip.addFill();
		topConfirmReadingToolStrip.addButton(topConfirmReadingButton);

		redrawing = true;
		addMember(topConfirmReadingToolStrip, 0);
		redrawing = false;
	}

	private Label prepareConfirmReadingLabel() {
		Label label = new Label(I18N.message("usersrequirereading"));
		List<GUIReadingRequest> unconfirmedReadings = ReadingRequestController.get()
				.getUnconfirmedReadings(document.getId());
		if (unconfirmedReadings.size() == 1)
			label = new Label(I18N.message("userrequiresreading", unconfirmedReadings.get(0).getRequestorName()));
		label.setWrap(false);
		label.setAlign(Alignment.LEFT);

		StringBuilder sb = new StringBuilder();
		for (GUIReadingRequest reading : unconfirmedReadings) {
			sb.append("<b>");
			sb.append(reading.getRequestorName());
			sb.append("</b>");
			if (reading.getRequestComment() != null && !reading.getRequestComment().isEmpty()) {
				sb.append(": ");
				sb.append(reading.getRequestComment());
			}
		}
		sb.append("<br />");
		label.setTooltip(sb.toString());
		return label;
	}

	private void showBottomConfirmReadingToolbar() {
		bottomConfirmReadingButton.addClickHandler(event -> confirmReading());
		bottomConfirmReadingButton.setDisabled(true);
		bottomConfirmReadingButton.setTooltip(I18N.message("readalldoctoenablebutton"));

		bottomConfirmReadingLabel = prepareConfirmReadingLabel();

		bottomConfirmReadingToolStrip = new ToolStrip();
		bottomConfirmReadingToolStrip.setWidth100();
		bottomConfirmReadingToolStrip.setAlign(Alignment.RIGHT);
		bottomConfirmReadingToolStrip.addMember(bottomConfirmReadingLabel);
		bottomConfirmReadingToolStrip.addFill();
		bottomConfirmReadingToolStrip.addButton(bottomConfirmReadingButton);

		redrawing = true;
		addMember(bottomConfirmReadingToolStrip, 2);
		redrawing = false;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	/**
	 * Declares the javascript function used to notify when the document has
	 * been completely read
	 * 
	 * @param previewPanel the preview panel
	 */
	public static native void declareOnReadingCompleted(PreviewPanel previewPanel) /*-{
		$wnd.onReadingCompleted = function() {
			return previewPanel.@com.logicaldoc.gui.common.client.preview.PreviewPanel::onReadingCompleted()();
		};
	}-*/;
}