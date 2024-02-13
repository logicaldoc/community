package com.logicaldoc.gui.common.client.dialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.validators.EmailValidator;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;

/**
 * A generic dialog box for sending emails
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public abstract class AbstractEmailDialog extends Window {

	private static final String EMAIL = "email";

	protected ValuesManager vm = new ValuesManager();

	protected DynamicForm form = new DynamicForm();

	protected ListGrid recipientsGrid;

	protected RichTextItem message;

	protected SelectItem from = ItemFactory.newEmailFromSelector("from", null);

	protected IButton sendButton = new IButton();

	protected String defaultMessage = null;

	protected AbstractEmailDialog() {
		super();

		addCloseClickHandler(event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setWidth(600);
		setHeight(430);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(false);
	}

	protected List<FormItem> prepareFormItems() {
		List<FormItem> fields = new ArrayList<>();
		fields.add(from);

		message = ItemFactory.newRichTextItemForEmail("message", "message", defaultMessage, null);
		message.setWidth("*");
		message.setHeight(200);
		message.setBrowserSpellCheck(true);
		message.setColSpan(2);
		fields.add(message);

		return fields;
	}

	protected void prepareEmail(GUIEmail mail) {
		// Nothing to do
	}

	@Override
	public void onDraw() {
		SectionStack recipientsStack = prepareRecipientsGrid();

		form.setValuesManager(vm);
		form.setWidth100();
		form.setHeight("*");
		form.setMargin(5);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(2);

		sendButton.setTitle(I18N.message("send"));
		sendButton.setMargin(3);
		sendButton.setHeight(30);
		sendButton.addClickHandler((ClickEvent event) -> {
			vm.validate();
			if (Boolean.TRUE.equals(vm.hasErrors()))
				return;

			GUIEmail mail = new GUIEmail();
			mail.setFrom(new GUIContact(from.getValueAsString()));
			mail.setMessage(message.getValue().toString());

			prepareEmail(mail);

			List<String> to = new ArrayList<>();
			List<String> cc = new ArrayList<>();
			List<String> bcc = new ArrayList<>();
			ListGridRecord[] records = recipientsGrid.getRecords();
			fillRecipients(to, cc, bcc, records);

			if (to.isEmpty() && cc.isEmpty()) {
				SC.warn(I18N.message("leastvalidrecipient"));
				return;
			}

			sendButton.disable();

			setRecipients(mail, to, cc, bcc);

			onSubmit(mail);
		});

		addItem(recipientsStack);
		addItem(form);
		addItem(prepareButtons());

		List<FormItem> items = prepareFormItems();
		form.setFields(items.toArray(new FormItem[0]));
	}

	protected HLayout prepareButtons() {
		HLayout buttons = new HLayout();
		buttons.setMembers(sendButton);
		buttons.setHeight(30);
		return buttons;
	}

	private void fillRecipients(List<String> to, List<String> cc, List<String> bcc, ListGridRecord[] records) {
		for (int i = 0; i < records.length; i++) {
			if (Boolean.TRUE.equals(recipientsGrid.validateCell(i, EMAIL))) {
				ListGridRecord rec = records[i];
				if (rec.getAttribute(EMAIL) == null || rec.getAttribute("type").trim().equals(""))
					continue;
				if ("to".equals(rec.getAttribute("type")))
					to.add(rec.getAttribute(EMAIL).trim());
				else if ("cc".equals(rec.getAttribute("type")))
					cc.add(rec.getAttribute(EMAIL).trim());
				else
					bcc.add(rec.getAttribute(EMAIL).trim());
			}
		}
	}

	private void setRecipients(GUIEmail mail, List<String> to, List<String> cc, List<String> bcc) {
		List<GUIContact> tos = new ArrayList<>();
		for (String email : to)
			tos.add(new GUIContact(email));
		mail.setTos(tos);

		List<GUIContact> ccs = new ArrayList<>();
		for (String email : cc)
			ccs.add(new GUIContact(email));
		mail.setCcs(ccs);

		List<GUIContact> bccs = new ArrayList<>();
		for (String email : bcc)
			bccs.add(new GUIContact(email));
		mail.setBccs(bccs);
	}

	protected abstract void onSubmit(GUIEmail mail);

	private SectionStack prepareRecipientsGrid() {
		SectionStack sectionStack = new SectionStack();
		sectionStack.setWidth100();
		sectionStack.setHeight(150);
		sectionStack.setMargin(6);

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("recipients") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);

		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL));
		email.setWidth("*");
		email.setCanFilter(true);
		FormItem emailItem = ItemFactory.newEmailComboSelector(EMAIL, EMAIL);
		emailItem.setRequired(true);
		emailItem.setWidth("*");
		// Delete the row
		emailItem.addKeyPressHandler(this::handleBackspace);
		email.setEditorProperties(emailItem);
		email.setValidators(new EmailValidator());

		ListGridField type = new ListGridField("type", I18N.message(" "));
		type.setCanFilter(true);
		type.setWidth(50);
		type.setCanEdit(true);
		type.setEditorProperties(ItemFactory.newRecipientTypeSelector("type"));

		recipientsGrid = new ListGrid();
		recipientsGrid.setShowRecordComponents(true);
		recipientsGrid.setShowRecordComponentsByCell(true);
		recipientsGrid.setAutoFetchData(true);
		recipientsGrid.setCanEdit(true);
		recipientsGrid.setShowHeader(false);
		recipientsGrid.setWidth100();
		recipientsGrid.setEditEvent(ListGridEditEvent.CLICK);
		recipientsGrid.setFields(type, email);

		recipientsGrid.addEditCompleteHandler(event -> addEmptyRow());

		recipientsGrid.addEditorExitHandler(event -> addEmptyRow());

		recipientsGrid.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (value == null)
				return null;
			if (colNum == 0)
				return I18N.message(value.toString());
			else
				return value.toString();
		});

		ListGridRecord rec = new ListGridRecord();
		rec.setAttribute("type", "to");
		rec.setAttribute(EMAIL, "");
		recipientsGrid.setRecords(new ListGridRecord[] { rec });

		final SelectItem contactsSelector = ItemFactory.newEmailSelector("contacts", "contacts");
		contactsSelector.setWidth(200);
		contactsSelector.addChangedHandler(event -> {
			ListGridRecord[] newSelection = contactsSelector.getSelectedRecords();
			if (newSelection == null || newSelection.length < 1)
				return;

			addSelectedRecipients(newSelection);
		});

		DynamicForm addressbook = new DynamicForm();
		addressbook.setItems(contactsSelector);

		section.setItems(recipientsGrid, addressbook);
		sectionStack.setSections(section);

		return sectionStack;
	}

	private void addSelectedRecipients(ListGridRecord[] newSelection) {
		for (int i = 0; i < newSelection.length; i++) {
			ListGridRecord newRec = new ListGridRecord();
			newRec.setAttribute(EMAIL, newSelection[i].getAttributeAsString(EMAIL));
			newRec.setAttribute("type", "to");

			addSelectedRecipient(newRec);
		}
	}

	private void addSelectedRecipient(ListGridRecord newRec) {
		// Iterate over the current recipients avoiding duplicates
		boolean duplicate = false;
		ListGridRecord[] currentRecipients = recipientsGrid.getRecords();
		for (int j = 0; j < currentRecipients.length; j++) {
			ListGridRecord rec = currentRecipients[j];
			if (rec.getAttributeAsString(EMAIL).contains(newRec.getAttributeAsString(EMAIL))) {
				duplicate = true;
				break;
			}
		}

		if (!duplicate) {
			// Iterate over the current recipients finding an empty
			// slot
			boolean empty = false;
			for (int j = 0; j < currentRecipients.length; j++) {
				if (currentRecipients[j].getAttributeAsString(EMAIL).isEmpty()) {
					empty = true;
					currentRecipients[j].setAttribute(EMAIL, newRec.getAttributeAsString(EMAIL));
					recipientsGrid.refreshRow(j);
					break;
				}
			}

			if (!empty)
				recipientsGrid.addData(newRec);
		}
	}

	private void handleBackspace(KeyPressEvent event) {
		if (event.getKeyName().equals("Backspace")) {
			ListGridRecord selection = recipientsGrid.getSelectedRecord();
			if ((selection.getAttribute(EMAIL) == null || selection.getAttribute(EMAIL).equals(""))
					&& recipientsGrid.getDataAsRecordList().getLength() > 1)
				recipientsGrid.removeSelectedData();
		}
	}

	private void addEmptyRow() {
		ListGridRecord[] records = recipientsGrid.getRecords();
		// Search for an empty rec
		for (ListGridRecord rec : records) {
			if (rec.getAttribute(EMAIL) == null || rec.getAttribute(EMAIL).trim().equals(""))
				return;
		}

		ListGridRecord[] newRecords = Arrays.copyOf(records, records.length + 1);
		newRecords[records.length] = new ListGridRecord();
		newRecords[records.length].setAttribute("type", "to");
		newRecords[records.length].setAttribute(EMAIL, "");
		recipientsGrid.setRecords(newRecords);
	}
}