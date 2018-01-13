package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager.DocumentProtectionHandler;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.HTMLPane;
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

	private MailPreviewPanel mail = null;

	private long docId;

	private String language;

	private boolean accessGranted = false;

	private GUIDocument document;

	private int width;

	private int height;

	public PreviewPanel(final GUIDocument document) {
		this.document = document;
		this.docId = document.getDocRef() != null ? document.getDocRef() : document.getId();
		this.language = Session.get().getUser().getLanguage();

		DocumentProtectionManager.askForPassword(docId, new DocumentProtectionHandler() {

			@Override
			public void onUnprotected(GUIDocument doc) {
				accessGranted = true;

				if (Util.isMediaFile(document.getFileName().toLowerCase())) {
					reloadMedia();
				} else if (document.getFileName().toLowerCase().endsWith(".html")
						|| document.getFileName().toLowerCase().endsWith(".htm")
						|| document.getFileName().toLowerCase().endsWith(".xhtml")) {
					reloadHTML();
				} else if (Util.isEmailFile(document.getFileName().toLowerCase())) {
					reloadMail();
				} else {
					reloadPreview(language);
				}

				redraw();
			}
		});

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				if (width != getWidth() || height != getHeight()) {
					width = getWidth();
					height = getHeight();
					redraw();
				}
			}
		});
	}

	public void redraw() {
		if (accessGranted) {
			if (preview != null) {
				removeMember(preview);
				if (getWidth() > 10)
					reloadPreview(language);
			} else if (html != null) {
				removeMember(html);
				if (getWidth() > 10)
					reloadHTML();
			} else if (media != null) {
				removeMember(media);
				if (getWidth() > 10)
					reloadMedia();
			} else if (mail != null) {
				removeMember(mail);
				if (getWidth() > 10)
					reloadMail();
			}
		}
	}

	/**
	 * Reloads a mail preview
	 */
	private void reloadMail() {
		DocumentService.Instance.get().extractEmail(docId, document.getFileVersion(), new AsyncCallback<GUIEmail>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
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
			Log.info(t.getMessage(), null);
		}

		media.setContents(contents);
		addMember(media);
	}

	/**
	 * Reloads a preview for HTML documents.
	 */
	private void reloadHTML() {
		html = new HTMLPane();
		html.setShowEdges(false);
		html.setContentsURL(Util.downloadURL(docId, document.getFileVersion(), "safe.html", false));
		html.setContentsType(ContentsType.FRAGMENT);

		setWidth100();
		addMember(html);
	}

	/**
	 * Reloads a preview.
	 */
	private void reloadPreview(String language) {
		preview = new HTMLFlow();
		String contents = "";

		try {
			String url = Util.contextPath() + "prev/index.jsp?docId=" + docId
					+ (document.getFileVersion() != null ? "&fileVersion=" + document.getFileVersion() : "")
					+ "&locale=" + I18N.getLocale();
			contents = "<iframe src='" + url + "' style='border:0px solid white; width:" + (getWidth() - 1)
					+ "px; height:" + (getHeight() - 1)
					+ "px; overflow:hidden;'  scrolling='no' seamless='seamless'></iframe>";
		} catch (Throwable t) {
		}

		preview.setContents(contents);
		addMember(preview);
	}
}
