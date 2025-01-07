package com.logicaldoc.gui.common.client.widgets.preview;

import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.FlowLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Renders the preview of a mail
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailPreviewPanel extends VLayout {

	private GUIEmail mail;

	private GUIDocument document;

	public MailPreviewPanel(GUIEmail mail, GUIDocument document, int width) {
		setWidth100();
		setHeight100();

		this.mail = mail;
		this.document = document;

		/**
		 * Prepare the form that contains the email data
		 */
		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setTitleAlign(Alignment.LEFT);
		form.setAlign(Alignment.LEFT);

		StaticTextItem from = prepareFromItem(mail);
		from.setEndRow(true);

		StaticTextItem subject = ItemFactory.newStaticTextItem("subject", mail.getSubject());
		subject.setEndRow(true);

		StaticTextItem replyto = prepareReplyToItem(mail);
		replyto.setEndRow(true);

		StaticTextItem sent = prepareSentItem(mail);

		StaticTextItem received = prepareReceivedItem(mail);

		StaticTextItem to = prepareToItem(mail);
		to.setEndRow(true);

		StaticTextItem cc = prepareCcItem(mail);
		cc.setEndRow(true);

		StaticTextItem bcc = prepareBccItem(mail);
		bcc.setEndRow(true);

		form.setItems(from, subject, replyto, to, cc, bcc, sent, received);

		VLayout header = preparePreviewHeader(mail, document, form);

		Canvas body = null;
		if (mail.getMessage().toLowerCase().startsWith("<html")
				|| mail.getMessage().toLowerCase().startsWith("<body")) {
			HTMLPane html = new HTMLPane();
			html.setWidth100();
			html.setHeight100();
			html.setContents("<iframe id='htmlmailpreview-" + getID()
					+ "' style='border:0px solid white; width:100%; height:100%;'></iframe>");
			body = html;
		} else {
			if (document.getFileName().toLowerCase().endsWith(".msg") && mail.getMessage().contains("\\rtf1")) {
				HTMLPane html = new HTMLPane();
				html.setWidth100();
				html.setHeight100();

				String contents = "";
				try {
					String url = Util.contextPath() + "prev/index.jsp?docId=" + document.getId()
							+ (document.getFileVersion() != null ? "&fileVersion=" + document.getFileVersion() : "")
							+ "&locale=" + I18N.getLocale();
					html.setIFrameURL(url);
					contents = "<iframe src='" + url
							+ "' style='border:0px solid white; width:100%; height:100%; overflow:hidden;' scrolling='no' seamless='seamless'></iframe>";
				} catch (Exception t) {
					// Nothing to do
				}
				html.setContents(contents);
				body = html;
			} else {
				TextAreaItem contentItem = ItemFactory.newTextAreaItem("content", mail.getMessage());
				contentItem.setShowTitle(false);
				contentItem.setWidth(width);
				contentItem.setHeight("*");

				form = new DynamicForm();
				form.setWidth100();
				form.setHeight100();
				form.setItems(contentItem);

				body = form;
			}
		}
		setMembers(header, body);
	}

	@Override
	protected void onDraw() {
		super.onDraw();

		String content = mail.getMessage();
		if (!content.startsWith("<html>"))
			content = "<!doctype html><head></head>" + content + "</html>";
		updateIframePreview("htmlmailpreview-" + getID(), content);
	}

	/**
	 * Javascript code to put the iframe content. We do not use the srcdoc=''
	 * because the first apostrophe inside the same content will interrupt the
	 * HTML
	 * 
	 * @param content the HTML content of the message
	 */
	private static native void updateIframePreview(String id, String content) /*-{
	    var frame = $wnd.document.getElementById(id);
	    if(frame!=null){
	    	var fdoc = frame.contentDocument;
			fdoc.write(content);
		}
	}-*/;

	/**
	 * Prepares the header that shows the most important informations about the
	 * email and the attachment buttons
	 * 
	 * @param mail The email
	 * @param document The document that represents the email
	 * @param form the form
	 * 
	 * @return the header layout
	 */
	private VLayout preparePreviewHeader(final GUIEmail mail, final GUIDocument document, DynamicForm form) {
		FlowLayout attachmentsPanel = new FlowLayout();
		attachmentsPanel.setWidth100();
		attachmentsPanel.setHeight(80);
		attachmentsPanel.setOverflow(Overflow.SCROLL);

		VLayout header = new VLayout();
		header.setWidth100();
		header.setBackgroundColor("#efefef;");

		List<GUIDocument> attachments = prepareAttachments(mail, document, attachmentsPanel);
		if (attachments.isEmpty())
			header.setMembers(form);
		else
			header.setMembers(form, attachmentsPanel);
		return header;
	}

	private List<GUIDocument> prepareAttachments(final GUIEmail mail, final GUIDocument document,
			FlowLayout attachmentsPanel) {
		List<GUIDocument> attachments = mail.getAttachments();
		for (final GUIDocument doc : attachments) {
			IButton button = new IButton(doc.getFileName() + " (" + Util.formatSizeCompact(doc.getFileSize()) + ")");
			button.setAutoFit(true);
			button.setIcon("[SKIN]/FileIcons/" + doc.getIcon() + ".svg");
			if (doc.getFolder().isDownload())
				button.addClickHandler(event -> {
					String filename = doc.getFileName();
					filename = filename.replace("&", "%26");
					filename = filename.replace(" ", "%20");
					filename = filename.replace("#", "%23");
					filename = filename.replace("/", "%2F");
					filename = filename.replace("=", "%3D");
					filename = filename.replace("?", "%3F");
					filename = filename.replace(":", "%3A");

					Util.download(Util.downloadAttachmentURL(document.getId(), document.getFileVersion(), filename));
				});
			button.setContextMenu(prepareButtonMenu(document, doc));
			attachmentsPanel.addTile(button);
		}
		return attachments;
	}

	private StaticTextItem prepareBccItem(final GUIEmail mail) {
		String bccString = mail.getBccs().stream().map(c -> c.displayString()).collect(Collectors.joining(", "));
		StaticTextItem bcc = ItemFactory.newStaticTextItem("bcc", bccString);
		bcc.setVisible(!bccString.isEmpty());
		return bcc;
	}

	private StaticTextItem prepareCcItem(final GUIEmail mail) {
		String ccString = mail.getCcs().stream().map(c -> c.displayString()).collect(Collectors.joining(", "));
		StaticTextItem cc = ItemFactory.newStaticTextItem("cc", ccString);
		cc.setVisible(!ccString.isEmpty());
		return cc;
	}

	private StaticTextItem prepareToItem(final GUIEmail mail) {
		String toString = mail.getTos().stream().map(c -> c.displayString()).collect(Collectors.joining(", "));
		StaticTextItem to = ItemFactory.newStaticTextItem("to", toString);
		to.setVisible(!toString.isEmpty());
		return to;
	}

	private StaticTextItem prepareReceivedItem(final GUIEmail mail) {
		StaticTextItem received = ItemFactory.newStaticTextItem("receiveddate",
				mail.getReceived() != null ? I18N.formatDate(mail.getReceived()) : null);
		received.setVisible(mail.getReceived() != null);
		return received;
	}

	private StaticTextItem prepareSentItem(final GUIEmail mail) {
		StaticTextItem sent = ItemFactory.newStaticTextItem("sentdate",
				mail.getSent() != null ? I18N.formatDate(mail.getSent()) : null);
		sent.setVisible(mail.getSent() != null);
		return sent;
	}

	private StaticTextItem prepareFromItem(final GUIEmail mail) {
		return ItemFactory.newStaticTextItem("from", mail.getFrom() != null ? mail.getFrom().displayLink() : "");
	}

	private StaticTextItem prepareReplyToItem(final GUIEmail mail) {
		String replyToString = mail.getReplyTo().stream().map(c -> c.displayString()).collect(Collectors.joining(", "));
		StaticTextItem replyto = ItemFactory.newStaticTextItem("replyto", replyToString);
		replyto.setVisible(!replyToString.isEmpty() && !replyToString.equals(mail.getFrom().getEmail()));
		return replyto;
	}

	private Menu prepareButtonMenu(final GUIDocument doc, final GUIDocument attachment) {
		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(event -> DocumentService.Instance.get().saveEmailAttachment(doc.getId(),
				doc.getFileVersion(), attachment.getFileName(), new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUIDocument doc) {
						if (MainPanel.get().isOnDocumentsTab() && FolderController.get().getCurrentFolder() != null)
							FolderNavigator.get().selectFolder(FolderController.get().getCurrentFolder().getId());
					}
				}));
		copy.setEnabled(doc.getFolder().isWrite());

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> Util
				.download(Util.downloadAttachmentURL(doc.getId(), doc.getFileVersion(), attachment.getFileName())));
		download.setEnabled(doc.getFolder().isDownload());

		Menu contextMenu = new Menu();
		contextMenu.setItems(download, copy);

		return contextMenu;
	}
	
	@Override
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}