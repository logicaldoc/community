package com.logicaldoc.gui.common.client.widgets.preview;

import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager.DocumentProtectionHandler;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel is used to show the document preview.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
public class PreviewPanel extends VLayout {

	private HTMLFlow preview = null;

	private HTMLFlow media = null;

	private HTMLFlow html = null;

	private HTMLFlow dicom = null;

	private MailPreviewPanel mail = null;

	private Canvas reload = null;

	private long docId;

	private boolean accessGranted = false;

	private GUIDocument document;

	private int width;

	private int height;

	private boolean redrawing = false;

	public PreviewPanel(final GUIDocument document) {
		this.document = document;
		this.docId = document.getDocRef() != null ? document.getDocRef() : document.getId();

		DocumentProtectionManager.askForPassword(docId, new DocumentProtectionHandler() {

			@Override
			public void onUnprotected(GUIDocument doc) {
				accessGranted = true;

				if (Util.isMediaFile(document.getFileName().toLowerCase())) {
					reloadMedia();
				} else if (Util.isWebContentFile(document.getFileName().toLowerCase())) {
					reloadHTML();
				} else if (Util.isEmailFile(document.getFileName().toLowerCase())) {
					reloadMail();
				} else if (Util.isDICOMFile(document.getFileName().toLowerCase())) {
					FolderService.Instance.get().getFolder(document.getFolder().getId(), false, false, false,
							new AsyncCallback<GUIFolder>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIFolder folder) {
									if (folder.isDownload())
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
		});

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				if (getWidth() < 10L) {
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
				} else if (!redrawing && (width != getWidth() || height != getHeight())) {
					width = getWidth();
					height = getHeight();
					clearContent();
					showReloadPanel();
				}
			}
		});
	}

	public synchronized void redraw() {
		redrawing = true;
		try {
			clearContent();
			width = getWidth();
			height = getHeight();
			if (accessGranted) {
				if (preview != null) {
					if (getWidth() > 10)
						reloadPreview();
				} else if (html != null) {
					if (getWidth() > 10)
						reloadHTML();
				} else if (dicom != null) {
					if (getWidth() > 10)
						reloadDICOM();
				} else if (media != null) {
					if (getWidth() > 10)
						reloadMedia();
				} else if (mail != null) {
					if (getWidth() > 10)
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
	private void reloadMail() {
		DocumentService.Instance.get().extractEmail(docId, document.getFileVersion(), new AsyncCallback<GUIEmail>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(final GUIEmail email) {
				if (mail != null)
					removeMember(mail);
				mail = new MailPreviewPanel(email, document, getWidth(), getHeight());
				addMember(mail);
			}
		});
	}

	/**
	 * Reloads a media preview
	 */
	private void reloadMedia() {
		if (media != null)
			removeMember(media);

		media = new HTMLFlow();
		String contents = "";

		try {
			String url = Util.downloadURL(docId, document.getFileVersion(), false);

			if (Util.isAudioFile(document.getFileName()))
				contents = Util.audioHTML(url);
			else
				contents = Util.videoHTML(url, getWidth() != null ? "" + (getWidth() - 2) : "",
						getHeight() != null ? "" + (getHeight() - 1) : "");
		} catch (Throwable t) {
			GuiLog.info(t.getMessage(), null);
		}

		media.setContents(contents);
		addMember(media);
	}

	/**
	 * Reloads a preview for HTML documents.
	 */
	private void reloadHTML() {
		if (html != null)
			removeMember(html);

		html = new HTMLPane();
		html.setShowEdges(false);
		html.setContentsURL(Util.downloadURL(docId, document.getFileVersion(), "safe.html", false));
		html.setContentsType(ContentsType.FRAGMENT);

		setWidth100();
		addMember(html);
	}

	/**
	 * Reloads a preview for DICOM documents.
	 */
	private void reloadDICOM() {
		if (dicom != null)
			removeMember(dicom);

		dicom = new HTMLFlow();
		String url = Util.contextPath() + "dicom/index.html?input=" + URL.encodeQueryString(
				Util.downloadURL(docId, document.getFileVersion()) + "&sid=" + Session.get().getSid());
		dicom.setContents(
				"<iframe src='" + url + "' style='border:0px solid white; width:" + (getWidth() - 1) + "px; height:"
						+ (getHeight() - 1) + "px; overflow:hidden;' scrolling='no' seamless='seamless'></iframe>");
		setWidth100();
		addMember(dicom);
	}

	/**
	 * Reloads a preview.
	 */
	private void reloadPreview() {
		if (preview != null)
			removeMember(preview);

		preview = new HTMLFlow();
		String contents = "";

		long maxFileSize = Session.get().getConfigAsLong("gui.preview.maxfilesize") * 1024 * 1024;
		if (maxFileSize > 0 && document.getFileSize() != null && maxFileSize < document.getFileSize()) {
			contents = "<div><br><center><b>" + I18N.message("doctoobigtoberendered") + "</b></center></br></div>";
		} else {
			try {
				String locale = Session.get().getUser().getLanguage();
				String url = Util.contextPath() + "prev/index.jsp?docId=" + docId
						+ (document.getFileVersion() != null ? "&fileVersion=" + document.getFileVersion() : "")
						+ "&control=preview&locale=" + locale + "#locale=" + locale.replace('_', '-');
				contents = "<iframe src='" + url + "' style='border:0px solid white; width:" + (getWidth() - 1)
						+ "px; height:" + (getHeight() - 1)
						+ "px; overflow:hidden;'  scrolling='no' seamless='seamless'></iframe>";
			} catch (Throwable t) {
			}
		}
		preview.setContents(contents);
		addMember(preview);
	}

	private void clearContent() {
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
	}

	private void showReloadPanel() {
		IButton reloadButton = new IButton(I18N.message("reload"));
		reloadButton.setAutoFit(true);
		reloadButton.setSnapTo("C");
		reloadButton.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				redraw();
			}
		});

		reload = new Canvas();
		reload.setWidth100();
		reload.setHeight100();
		reload.setAlign(Alignment.CENTER);
		reload.addChild(reloadButton);

		addMember(reload);
	}
}
