package com.logicaldoc.gui.frontend.client.document;

import java.util.LinkedHashMap;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.common.client.dialogs.AbstractEmailDialog;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * This is the form used to send emails and download tickets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailDialog extends AbstractEmailDialog {

	private static final String SIGNATURE_SEPARATOR = "--";

	private long[] docIds;

	private String docTitle;

	private TextItem subject;

	private CheckboxItem pdf;

	private CheckboxItem ticket;

	private CheckboxItem zip;

	private SelectItem messageTemplate;

	public EmailDialog(long[] docIds, final String docTitle) {
		super();
		this.docIds = docIds;
		this.docTitle = docTitle;

		setHeight(570);
		setTitle(I18N.message("sendmail"));
	}

	@Override
	protected List<FormItem> prepareFormItems() {
		List<FormItem> fields = super.prepareFormItems();

		subject = ItemFactory.newTextItemForAutomation("subject", "subject", docTitle, null);
		subject.setBrowserSpellCheck(true);
		subject.setRequired(true);
		subject.setWidth(350);

		messageTemplate = ItemFactory.newSelectItem("template", "messagetemplate");
		messageTemplate.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if (messageTemplate.getValueAsString() != null && !"".equals(messageTemplate.getValueAsString())) {
					MessageService.Instance.get().getTemplate(Long.parseLong(messageTemplate.getValueAsString()),
							new AsyncCallback<GUIMessageTemplate>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIMessageTemplate t) {
									GuiLog.info(t.getSubject());

									subject.setValue(t.getSubject());
									message.setValue(t.getBody());
									updateSignature();
								}
							});
				} else {
					subject.setValue(docTitle);
				}
			}
		});

		pdf = new CheckboxItem("pdf");
		pdf.setTitle(I18N.message("sendpdfconversion"));
		pdf.setVisible(Feature.enabled(Feature.PDF));
		pdf.setColSpan(2);

		ticket = new CheckboxItem("sendticket");
		ticket.setTitle(I18N.message("sendticket"));
		ticket.setColSpan(2);
		ticket.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if ((ticket.getValue() != null && ticket.getValueAsBoolean()) || !Feature.enabled(Feature.PDF)) {
					pdf.setValue(false);
					pdf.hide();
					appendDownloadTicketPlaceholder();
				} else {
					pdf.show();
					removeDownloadTicketPlaceholder();
				}
			}
		});

		zip = new CheckboxItem();
		zip.setName("zip");
		zip.setTitle(I18N.message("zipattachments"));

		fields.add(1, messageTemplate);
		fields.add(2, subject);
		if (docIds.length == 1)
			fields.add(3, ticket);
		else
			fields.add(3, zip);
		fields.add(4, pdf);

		return fields;
	}

	@Override
	protected void prepareEmail(GUIEmail mail) {
		mail.setSubject(subject.getValueAsString());
		mail.setSendAsTicket(ticket.getValue() != null && ticket.getValueAsBoolean());
		mail.setPdfConversion(pdf.getValue() != null && pdf.getValueAsBoolean());
		mail.setZipCompression(zip.getValue() != null && zip.getValueAsBoolean());
		mail.setDocIds(docIds);
	}

	public void onDraw() {
		super.onDraw();

		from.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				updateSignature();
			}
		});

		updateSignature();

		MessageService.Instance.get().loadTemplates(I18N.getLocale(), "user",
				new AsyncCallback<GUIMessageTemplate[]>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIMessageTemplate[] templates) {
						LinkedHashMap<String, String> map = new LinkedHashMap<>();
						map.put("", "");
						for (GUIMessageTemplate t : templates)
							map.put("" + t.getId(), t.getName());
						messageTemplate.setValueMap(map);
						messageTemplate.setValue("");
					}
				});
	}

	@Override
	protected void onSend(GUIEmail mail) {
		LD.contactingServer();
		DocumentService.Instance.get().sendAsEmail(mail, Session.get().getUser().getLanguage(),
				new AsyncCallback<String>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
						sendButton.enable();
						destroy();
					}

					@Override
					public void onSuccess(String result) {
						LD.clearPrompt();
						sendButton.enable();
						if ("ok".equals(result)) {
							GuiLog.info(I18N.message("messagesent") + ". " + I18N.message("documentcopysent"));
						} else {
							GuiLog.error(I18N.message("messagenotsent"), null, null);
						}
						destroy();
					}
				});
	}

	private String getMessageWithoutSignature() {
		String currentMessage = message.getValue() != null ? message.getValue().toString() : "";
		int index = currentMessage.lastIndexOf(SIGNATURE_SEPARATOR);
		if (index > 0)
			currentMessage = currentMessage.substring(0, index);
		return currentMessage;
	}

	private void appendDownloadTicketPlaceholder() {
		removeDownloadTicketPlaceholder();
		String messageBody = getMessageWithoutSignature();
		messageBody += "<br />$downloadTicket<br />";
		message.setValue(messageBody);
		updateSignature();
	}

	private void removeDownloadTicketPlaceholder() {
		String messageBody = getMessageWithoutSignature().replaceAll("(<br />)?\\$downloadTicket(<br />)?", "")
				.replace("\\$downloadTicket", "");
		message.setValue(messageBody);
		updateSignature();
	}

	/**
	 * Updates the signature at the end of the message
	 */
	private void updateSignature() {
		String sgn = Session.get().getUser().getEmailSignature();
		if (!from.getValue().equals(Session.get().getUser().getEmail()))
			sgn = Session.get().getUser().getEmailSignature2();

		String currentMessage = getMessageWithoutSignature();

		if (!currentMessage.endsWith("<br />"))
			currentMessage += "<br /><br />";

		if (sgn != null && !"".equals(sgn.trim()))
			message.setValue(currentMessage + SIGNATURE_SEPARATOR + "<br />" + sgn);
		else
			message.setValue(currentMessage);
	}
}