package com.logicaldoc.gui.frontend.client.account.contacts;

import java.util.LinkedHashMap;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIParseContactsParameters;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.ContactService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;

/**
 * This popup window is used to upload a new contacts file to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0.1
 */
public class ContactsImportSettings extends Window {

	private static final String COLUMNINDEX = "columnindex";

	private DynamicForm form;

	public ContactsImportSettings() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadcontacts"));
		setWidth(380);
		setHeight(330);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		form = new DynamicForm();

		SelectItem separated = ItemFactory.newSelectItem("separatedby");
		separated.setWidth(80);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put(",", ",");
		map.put(";", ";");
		separated.setValueMap(map);
		separated.setValue(",");

		SelectItem delimiter = ItemFactory.newSelectItem("textdelimiter");
		delimiter.setWidth(80);
		LinkedHashMap<String, String> map2 = new LinkedHashMap<>();
		map2.put("\"", "\"");
		map2.put("'", "'");
		delimiter.setValueMap(map2);
		delimiter.setValue("\"");

		ToggleItem skip = ItemFactory.newToggleItem("skipfirstrow", true);

		SpinnerItem firstName = ItemFactory.newSpinnerItem("firstname", 1);
		firstName.setRequired(true);
		firstName.setWidth(60);
		firstName.setMin(1);
		firstName.setHint(I18N.message(COLUMNINDEX));

		SpinnerItem lastName = ItemFactory.newSpinnerItem("lastname", 2);
		lastName.setRequired(true);
		lastName.setWidth(60);
		lastName.setMin(1);
		lastName.setHint(I18N.message(COLUMNINDEX));

		SpinnerItem email = ItemFactory.newSpinnerItem("email", 3);
		email.setRequired(true);
		email.setWidth(60);
		email.setMin(1);
		email.setHint(I18N.message(COLUMNINDEX));

		SpinnerItem company = ItemFactory.newSpinnerItem("company", 4);
		company.setRequired(true);
		company.setWidth(60);
		company.setMin(1);
		company.setHint(I18N.message(COLUMNINDEX));

		SpinnerItem phone = ItemFactory.newSpinnerItem("phone", 5);
		phone.setRequired(true);
		phone.setWidth(60);
		phone.setMin(1);
		phone.setHint(I18N.message(COLUMNINDEX));

		SpinnerItem mobile = ItemFactory.newSpinnerItem("mobile", "cell", 6);
		mobile.setRequired(true);
		mobile.setWidth(60);
		mobile.setMin(1);
		mobile.setHint(I18N.message(COLUMNINDEX));

		SpinnerItem address = ItemFactory.newSpinnerItem("address", 7);
		address.setRequired(true);
		address.setWidth(60);
		address.setMin(1);
		address.setHint(I18N.message(COLUMNINDEX));

		SubmitItem importButton = new SubmitItem();
		importButton.setTitle(I18N.message("iimport"));
		importButton.setEndRow(true);
		importButton.addClickHandler(event -> onImport());

		form.setItems(separated, delimiter, skip, firstName, lastName, email, company, phone, mobile, address,
				importButton);

		addItem(form);
	}

	public String getSeparator() {
		return form.getValues().get("separatedby").toString();
	}

	public String getTextDelimiter() {
		return form.getValues().get("textdelimiter").toString();
	}

	public boolean isSkipFirstRow() {
		return Boolean.valueOf(form.getValueAsString("skipfirstrow"));
	}

	public int getFirstNameIndex() {
		return Integer.parseInt(form.getValues().get("firstname").toString());
	}

	public int getLastNameIndex() {
		return Integer.parseInt(form.getValues().get("lastname").toString());
	}

	public int getEmailIndex() {
		return Integer.parseInt(form.getValues().get("email").toString());
	}

	public int getCompanyIndex() {
		return Integer.parseInt(form.getValues().get("company").toString());
	}

	public int getPhoneIndex() {
		return Integer.parseInt(form.getValues().get("phone").toString());
	}

	public int getMobileIndex() {
		return Integer.parseInt(form.getValues().get("mobile").toString());
	}

	public int getAddressIndex() {
		return Integer.parseInt(form.getValues().get("address").toString());
	}

	public GUIParseContactsParameters getParseContactsParameters() {
		GUIParseContactsParameters parameters = new GUIParseContactsParameters(getSeparator(), getTextDelimiter(),
				isSkipFirstRow());
		parameters.setAddress(getAddressIndex());
		parameters.setCompany(getCompanyIndex());
		parameters.setEmail(getEmailIndex());
		parameters.setFirstName(getFirstNameIndex());
		parameters.setLastName(getLastNameIndex());
		parameters.setMobile(getMobileIndex());
		parameters.setPhone(getPhoneIndex());
		return parameters;
	}

	private void onImport() {
		if (form.validate()) {
			LD.contactingServer();
			try {
				ContactService.Instance.get().parseContacts(true, getParseContactsParameters(), new DefaultAsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						LD.clearPrompt();
					}

					@Override
					public void onSuccess(List<GUIContact> contacts) {
						LD.clearPrompt();
						ContactsImportPreview preview = new ContactsImportPreview(ContactsImportSettings.this);
						preview.show();
						preview.setContacts(contacts);
					}
				});
			} catch (Exception t) {
				LD.clearPrompt();
			}
		}
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}