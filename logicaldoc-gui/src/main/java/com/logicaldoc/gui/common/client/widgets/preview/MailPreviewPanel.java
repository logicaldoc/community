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
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.FlowLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Renders the preview of a mail
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class MailPreviewPanel extends VLayout {

	public MailPreviewPanel(final GUIEmail mail, final GUIDocument document, final int width, final int height) {
		setWidth100();
		setHeight100();

		/**
		 * Prepare the form that contains the email data
		 */
		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setTitleAlign(Alignment.LEFT);
		form.setAlign(Alignment.LEFT);

		StaticTextItem from = ItemFactory.newStaticTextItem("from", "from",
				mail.getFrom() != null ? mail.getFrom().displayLink() : "");
		from.setEndRow(true);

		StaticTextItem subject = ItemFactory.newStaticTextItem("subject", "subject", mail.getSubject());
		subject.setEndRow(true);

		String replyToString = "";
		if (mail.getReplyTo() != null && mail.getReplyTo().length > 0)
			for (GUIContact contact : mail.getReplyTo()) {
				if (!replyToString.isEmpty())
					replyToString += ", ";
				replyToString += contact.displayLink();
			}
		StaticTextItem replyto = ItemFactory.newStaticTextItem("replyto", "replyto", replyToString);
		replyto.setEndRow(true);
		replyto.setVisible(!replyToString.isEmpty() && !replyToString.equals(mail.getFrom().getEmail()));

		StaticTextItem sent = ItemFactory.newStaticTextItem("sentdate", "sentdate",
				mail.getSent() != null ? I18N.formatDate(mail.getSent()) : null);
		sent.setVisible(mail.getSent() != null);

		StaticTextItem received = ItemFactory.newStaticTextItem("receiveddate", "receiveddate",
				mail.getReceived() != null ? I18N.formatDate(mail.getReceived()) : null);
		received.setVisible(mail.getReceived() != null);

		String toString = "";
		if (mail.getTos() != null && mail.getTos().length > 0)
			for (GUIContact contact : mail.getTos()) {
				if (!toString.isEmpty())
					toString += ", ";
				toString += contact.displayLink();
			}
		StaticTextItem to = ItemFactory.newStaticTextItem("to", "to", toString);
		to.setEndRow(true);
		to.setVisible(!toString.isEmpty());

		String ccString = "";
		if (mail.getCcs() != null && mail.getCcs().length > 0)
			for (GUIContact contact : mail.getCcs()) {
				if (!ccString.isEmpty())
					ccString += ", ";
				ccString += contact.displayLink();
			}
		StaticTextItem cc = ItemFactory.newStaticTextItem("cc", "cc", ccString);
		cc.setEndRow(true);
		cc.setVisible(!ccString.isEmpty());

		String bccString = "";
		if (mail.getBccs() != null && mail.getBccs().length > 0)
			for (GUIContact contact : mail.getBccs()) {
				if (!bccString.isEmpty())
					bccString += ", ";
				bccString += contact.displayLink();
			}
		StaticTextItem bcc = ItemFactory.newStaticTextItem("bcc", "bcc", bccString);
		bcc.setEndRow(true);
		bcc.setVisible(!bccString.isEmpty());

		form.setItems(from, subject, replyto, to, cc, bcc, sent, received);

		FlowLayout attachmentsFlow = new FlowLayout();
		attachmentsFlow.setWidth100();
		attachmentsFlow.setHeight(75);

		GUIDocument[] docs = mail.getAttachments();
		if (docs != null)
			for (final GUIDocument doc : docs) {
				IButton button = new IButton(
						doc.getFileName() + " (" + Util.formatSizeCompact(doc.getFileSize()) + ")");
				button.setAutoFit(true);
				button.setIcon("[SKIN]/" + doc.getIcon());
				if (doc.getFolder().isDownload())
					button.addClickHandler(new ClickHandler() {

						@Override
						public void onClick(ClickEvent event) {
							String url = Util.downloadAttachmentURL(document.getId(), document.getFileVersion(),
									doc.getFileName());
							Util.download(url);
						}
					});
				button.setContextMenu(prepareButtonMenu(document, doc));
				attachmentsFlow.addTile(button);
			}

		VLayout header = new VLayout();
		header.setWidth100();
		header.setBackgroundColor("#efefef;");

		if (docs != null && docs.length > 0)
			header.setMembers(form, attachmentsFlow);
		else
			header.setMembers(form);

		Canvas body = null;
		if (mail.getMessage().toLowerCase().startsWith("<html")
				|| mail.getMessage().toLowerCase().startsWith("<body")) {
			HTMLPane htmlBody = new HTMLPane();
			htmlBody.setShowEdges(true);
			htmlBody.setWidth100();
			htmlBody.setHeight100();
			htmlBody.setContents(mail.getMessage());
			htmlBody.setShowEdges(false);
			body = htmlBody;
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
							+ "' style='border:0px solid white; width:100%; height:100%; overflow:hidden;'  scrolling='no' seamless='seamless'></iframe>";
				} catch (Throwable t) {
					// Nothing to do
				}
				html.setContents(contents);
				body = html;
			} else {
				TextAreaItem contentItem = ItemFactory.newTextAreaItem("content", I18N.message("content"),
						mail.getMessage());
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

	private Menu prepareButtonMenu(final GUIDocument doc, final GUIDocument attachment) {
		MenuItem copy = new MenuItem();
		copy.setTitle(I18N.message("copy"));
		copy.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				DocumentService.Instance.get().saveEmailAttachment(doc.getId(), doc.getFileVersion(),
						attachment.getFileName(), new AsyncCallback<GUIDocument>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument doc) {
								if (MainPanel.get().isOnDocumentsTab())
									if (FolderController.get().getCurrentFolder() != null)
										FolderNavigator.get()
												.selectFolder(FolderController.get().getCurrentFolder().getId());
							}
						});
			}
		});
		copy.setEnabled(doc.getFolder().isWrite());

		MenuItem download = new MenuItem();
		download.setTitle(I18N.message("download"));
		download.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				String url = Util.downloadAttachmentURL(doc.getId(), doc.getFileVersion(), attachment.getFileName());
				Util.download(url);
			}
		});
		download.setEnabled(doc.getFolder().isDownload());

		Menu contextMenu = new Menu();
		contextMenu.setItems(download, copy);

		return contextMenu;
	}
}