package com.logicaldoc.gui.common.client.widgets.preview;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
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

	public MailPreviewPanel(final GUIEmail mail, final GUIDocument document, final int width) {
		setWidth100();
		setHeight100();

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
			html.setContents("<iframe style='border:0px solid white; width:100%; height:100%;' sandbox srcdoc='"
					+ mail.getMessage() + "'></iframe>");
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

	/**
	 * Prepares the header that shows the most important informations about the
	 * email and the attachment buttons
	 * 
	 * @param mail The email
	 * @param document The document that represents the emil
	 * @param form the form
	 * 
	 * @return the header layout
	 */
	private VLayout preparePreviewHeader(final GUIEmail mail, final GUIDocument document, DynamicForm form) {
		FlowLayout attachmentsPanel = new FlowLayout();
		attachmentsPanel.setWidth100();
		attachmentsPanel.setHeight(75);

		GUIDocument[] docs = prepareAttachments(mail, document, attachmentsPanel);

		VLayout header = new VLayout();
		header.setWidth100();
		header.setBackgroundColor("#efefef;");
		if (docs != null && docs.length > 0)
			header.setMembers(form, attachmentsPanel);
		else
			header.setMembers(form);
		return header;
	}

	private GUIDocument[] prepareAttachments(final GUIEmail mail, final GUIDocument document,
			FlowLayout attachmentsPanel) {
		GUIDocument[] docs = mail.getAttachments();
		if (docs != null)
			for (final GUIDocument doc : docs) {
				IButton button = new IButton(
						doc.getFileName() + " (" + Util.formatSizeCompact(doc.getFileSize()) + ")");
				button.setAutoFit(true);
				button.setIcon("[SKIN]/" + doc.getIcon());
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
		return docs;
	}

	private StaticTextItem prepareBccItem(final GUIEmail mail) {
		String bccString = "";
		if (mail.getBccs() != null && mail.getBccs().length > 0)
			for (GUIContact contact : mail.getBccs()) {
				if (!bccString.isEmpty())
					bccString += ", ";
				bccString += contact.displayLink();
			}
		StaticTextItem bcc = ItemFactory.newStaticTextItem("bcc", bccString);
		bcc.setVisible(!bccString.isEmpty());
		return bcc;
	}

	private StaticTextItem prepareCcItem(final GUIEmail mail) {
		String ccString = "";
		if (mail.getCcs() != null && mail.getCcs().length > 0)
			for (GUIContact contact : mail.getCcs()) {
				if (!ccString.isEmpty())
					ccString += ", ";
				ccString += contact.displayLink();
			}
		StaticTextItem cc = ItemFactory.newStaticTextItem("cc", ccString);
		cc.setVisible(!ccString.isEmpty());
		return cc;
	}

	private StaticTextItem prepareToItem(final GUIEmail mail) {
		String toString = "";
		if (mail.getTos() != null && mail.getTos().length > 0)
			for (GUIContact contact : mail.getTos()) {
				if (!toString.isEmpty())
					toString += ", ";
				toString += contact.displayLink();
			}
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
		return ItemFactory.newStaticTextItem("from",
				mail.getFrom() != null ? mail.getFrom().displayLink() : "");
	}

	private StaticTextItem prepareReplyToItem(final GUIEmail mail) {
		String replyToString = "";
		if (mail.getReplyTo() != null && mail.getReplyTo().length > 0)
			for (GUIContact contact : mail.getReplyTo()) {
				if (!replyToString.isEmpty())
					replyToString += ", ";
				replyToString += contact.displayLink();
			}
		StaticTextItem replyto = ItemFactory.newStaticTextItem("replyto", replyToString);
		replyto.setVisible(!replyToString.isEmpty() && !replyToString.equals(mail.getFrom().getEmail()));
		return replyto;
	}

	private Menu prepareButtonMenu(final GUIDocument doc, final GUIDocument attachment) {
		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(event -> 
			DocumentService.Instance.get().saveEmailAttachment(doc.getId(), doc.getFileVersion(),
					attachment.getFileName(), new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument doc) {
							if (MainPanel.get().isOnDocumentsTab() && FolderController.get().getCurrentFolder() != null)
								FolderNavigator.get().selectFolder(FolderController.get().getCurrentFolder().getId());
						}
					}));
		copy.setEnabled(doc.getFolder().isWrite());

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(event -> {
			String url = Util.downloadAttachmentURL(doc.getId(), doc.getFileVersion(), attachment.getFileName());
			Util.download(url);
		});
		download.setEnabled(doc.getFolder().isDownload());

		Menu contextMenu = new Menu();
		contextMenu.setItems(download, copy);

		return contextMenu;
	}
}