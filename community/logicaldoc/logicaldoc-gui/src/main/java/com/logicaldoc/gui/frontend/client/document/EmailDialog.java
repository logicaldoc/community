package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.validators.EmailValidator;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.grid.events.EditorExitEvent;
import com.smartgwt.client.widgets.grid.events.EditorExitHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;

/**
 * This is the form used to send emails and download tickets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailDialog extends Window {

	private static final String SIGNATURE_SEPARATOR = "--";

	private long[] docIds;

	private ValuesManager vm = new ValuesManager();

	private ListGrid recipientsGrid;

	private RichTextItem message;

	final SelectItem from = ItemFactory.newEmailFromSelector("from", null);

	public EmailDialog(long[] docIds, final String docTitle) {
		super();
		this.docIds = docIds;

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("sendmail"));
		setWidth(580);
		setHeight(570);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(false);

		SectionStack recipientsStack = prepareRecipientsGrid();

		final DynamicForm form = new DynamicForm();
		form.setID("emailform");
		form.setValuesManager(vm);
		form.setWidth100();
		form.setHeight("*");
		form.setMargin(5);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(2);

		final TextItem subject = ItemFactory.newTextItemForAutomation("subject", "subject", docTitle, null);
		subject.setBrowserSpellCheck(true);
		subject.setRequired(true);
		subject.setWidth(350);

		message = ItemFactory.newRichTextItemForEmail("message", "message", null, null);
		message.setWidth("*");
		message.setHeight(200);
		message.setBrowserSpellCheck(true);
		message.setColSpan(2);

		from.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				updateSignature();
			}
		});

		final SelectItem messageTemplate = ItemFactory.newSelectItem("template", "messagetemplate");
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

		updateSignature();

		final CheckboxItem pdf = new CheckboxItem("pdf");
		pdf.setTitle(I18N.message("sendpdfconversion"));
		pdf.setVisible(Feature.enabled(Feature.PDF));
		pdf.setColSpan(2);

		final CheckboxItem ticket = new CheckboxItem("sendticket");
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

		final CheckboxItem zip = new CheckboxItem();
		zip.setName("zip");
		zip.setTitle(I18N.message("zipattachments"));

		// The download ticket is available on single selection only
		if (docIds.length == 1)
			form.setFields(from, messageTemplate, subject, ticket, pdf, message);
		else
			form.setFields(from, messageTemplate, subject, zip, pdf, message);

		final IButton send = new IButton();
		send.setTitle(I18N.message("send"));
		send.setMargin(3);
		send.setHeight(30);
		send.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				vm.validate();
				if (!vm.hasErrors()) {
					GUIEmail mail = new GUIEmail();
					mail.setFrom(new GUIContact(from.getValueAsString()));
					mail.setSubject(subject.getValueAsString());
					mail.setMessage(message.getValue().toString());
					mail.setSendAsTicket(ticket.getValue() != null && ticket.getValueAsBoolean());
					mail.setPdfConversion(pdf.getValue() != null && pdf.getValueAsBoolean());
					mail.setZipCompression(zip.getValue() != null && zip.getValueAsBoolean());
					mail.setDocIds(EmailDialog.this.docIds);

					List<String> to = new ArrayList<String>();
					List<String> cc = new ArrayList<String>();
					List<String> bcc = new ArrayList<String>();
					ListGridRecord[] records = recipientsGrid.getRecords();
					for (int i = 0; i < records.length; i++) {
						if (!recipientsGrid.validateCell(i, "email"))
							continue;

						ListGridRecord record = records[i];
						if (record.getAttribute("email") == null || record.getAttribute("type").trim().equals(""))
							continue;
						if ("to".equals(record.getAttribute("type")))
							to.add(record.getAttribute("email").trim());
						else if ("cc".equals(record.getAttribute("type")))
							cc.add(record.getAttribute("email").trim());
						else
							bcc.add(record.getAttribute("email").trim());
					}

					if (to.isEmpty() && cc.isEmpty()) {
						SC.warn(I18N.message("leastvalidrecipient"));
						return;
					}

					send.disable();

					List<GUIContact> tos = new ArrayList<GUIContact>();
					for (String email : to)
						tos.add(new GUIContact(email));
					mail.setTos(tos.toArray(new GUIContact[0]));

					List<GUIContact> ccs = new ArrayList<GUIContact>();
					for (String email : cc)
						ccs.add(new GUIContact(email));
					mail.setCcs(ccs.toArray(new GUIContact[0]));

					List<GUIContact> bccs = new ArrayList<GUIContact>();
					for (String email : bcc)
						bccs.add(new GUIContact(email));
					mail.setBccs(bccs.toArray(new GUIContact[0]));

					LD.contactingServer();

					DocumentService.Instance.get().sendAsEmail(mail, Session.get().getUser().getLanguage(),
							new AsyncCallback<String>() {
								@Override
								public void onFailure(Throwable caught) {
									LD.clearPrompt();
									GuiLog.serverError(caught);
									send.enable();
									destroy();
								}

								@Override
								public void onSuccess(String result) {
									LD.clearPrompt();
									send.enable();
									if ("ok".equals(result)) {
										GuiLog.info(
												I18N.message("messagesent") + ". " + I18N.message("documentcopysent"));
									} else {
										GuiLog.error(I18N.message("messagenotsent"), null, null);
									}
									destroy();
								}
							});
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembers(send);
		buttons.setHeight(30);

		addItem(recipientsStack);
		addItem(form);
		addItem(buttons);

		MessageService.Instance.get().loadTemplates(I18N.getLocale(), "user",
				new AsyncCallback<GUIMessageTemplate[]>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIMessageTemplate[] templates) {
						LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
						map.put("", "");
						for (GUIMessageTemplate t : templates)
							map.put("" + t.getId(), t.getName());
						messageTemplate.setValueMap(map);
						messageTemplate.setValue("");
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
				.replaceAll("\\$downloadTicket", "");
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

		message.setValue(currentMessage + SIGNATURE_SEPARATOR + "<br />" + (sgn != null ? sgn : ""));
	}

	private SectionStack prepareRecipientsGrid() {
		SectionStack sectionStack = new SectionStack();
		sectionStack.setWidth100();
		sectionStack.setHeight(150);
		sectionStack.setMargin(6);

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("recipients") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);

		ListGridField email = new ListGridField("email", I18N.message("email"));
		email.setWidth("*");
		email.setCanFilter(true);
		FormItem emailItem = ItemFactory.newEmailComboSelector("email", "email");
		emailItem.setRequired(true);
		emailItem.setWidth("*");
		emailItem.addKeyPressHandler(new KeyPressHandler() {

			@Override
			public void onKeyPress(KeyPressEvent event) {
				// Delete the row
				if (event.getKeyName().equals("Backspace")) {
					ListGridRecord selection = recipientsGrid.getSelectedRecord();
					if (selection.getAttribute("email") == null
							|| selection.getAttribute("email").toString().equals(""))
						if (recipientsGrid.getDataAsRecordList().getLength() > 1)
							recipientsGrid.removeSelectedData();
				}

			}
		});
		email.setEditorType(emailItem);
		email.setValidators(new EmailValidator());

		ListGridField type = new ListGridField("type", I18N.message(" "));
		type.setCanFilter(true);
		type.setWidth(50);
		type.setCanEdit(true);
		type.setEditorType(ItemFactory.newRecipientTypeSelector("type"));

		recipientsGrid = new ListGrid();
		recipientsGrid.setShowRecordComponents(true);
		recipientsGrid.setShowRecordComponentsByCell(true);
		recipientsGrid.setAutoFetchData(true);
		recipientsGrid.setCanEdit(true);
		recipientsGrid.setShowHeader(false);
		recipientsGrid.setWidth100();
		recipientsGrid.setEditEvent(ListGridEditEvent.CLICK);
		recipientsGrid.setFields(type, email);

		recipientsGrid.addEditCompleteHandler(new EditCompleteHandler() {

			@Override
			public void onEditComplete(EditCompleteEvent event) {
				addEmptyRow();
			}
		});

		recipientsGrid.addEditorExitHandler(new EditorExitHandler() {

			@Override
			public void onEditorExit(EditorExitEvent event) {
				addEmptyRow();
			}
		});

		recipientsGrid.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if (value == null)
					return null;
				if (colNum == 0)
					return I18N.message(value.toString());
				else
					return value.toString();
			}
		});

		ListGridRecord record = new ListGridRecord();
		record.setAttribute("type", "to");
		record.setAttribute("email", "");
		recipientsGrid.setRecords(new ListGridRecord[] { record });

		final SelectItem contactsSelector = ItemFactory.newEmailSelector("contacts", "contacts");
		contactsSelector.setWidth(200);
		contactsSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord[] newSelection = contactsSelector.getSelectedRecords();
				if (newSelection == null || newSelection.length < 1)
					return;

				for (int i = 0; i < newSelection.length; i++) {
					ListGridRecord newRec = new ListGridRecord();
					newRec.setAttribute("email", newSelection[i].getAttributeAsString("email"));
					newRec.setAttribute("type", "to");

					// Iterate over the current recipients avoiding duplicates
					boolean duplicate = false;
					ListGridRecord[] currentRecipients = recipientsGrid.getRecords();
					for (int j = 0; j < currentRecipients.length; j++) {
						ListGridRecord rec = currentRecipients[j];
						if (rec.getAttributeAsString("email").contains(newRec.getAttributeAsString("email"))) {
							duplicate = true;
							break;
						}
					}

					if (!duplicate) {
						// Iterate over the current recipients finding an empty
						// slot
						boolean empty = false;
						for (int j = 0; j < currentRecipients.length; j++) {
							if (currentRecipients[j].getAttributeAsString("email").isEmpty()) {
								empty = true;
								currentRecipients[j].setAttribute("email", newRec.getAttributeAsString("email"));
								recipientsGrid.refreshRow(j);
								break;
							}
						}

						if (!empty)
							recipientsGrid.addData(newRec);
					}
				}
			}
		});

		DynamicForm addressbook = new DynamicForm();
		addressbook.setItems(contactsSelector);

		section.setItems(recipientsGrid, addressbook);
		sectionStack.setSections(section);

		return sectionStack;
	}

	private void addEmptyRow() {
		ListGridRecord[] records = recipientsGrid.getRecords();
		// Search for an empty record
		for (ListGridRecord rec : records) {
			if (rec.getAttribute("email") == null || rec.getAttribute("email").trim().equals(""))
				return;
		}

		ListGridRecord[] newRecords = new ListGridRecord[records.length + 1];
		for (int i = 0; i < records.length; i++)
			newRecords[i] = records[i];
		newRecords[records.length] = new ListGridRecord();
		newRecords[records.length].setAttribute("type", "to");
		newRecords[records.length].setAttribute("email", "");
		recipientsGrid.setRecords(newRecords);
	}
}