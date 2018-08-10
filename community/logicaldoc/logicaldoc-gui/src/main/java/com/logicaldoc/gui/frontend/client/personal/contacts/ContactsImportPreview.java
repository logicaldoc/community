package com.logicaldoc.gui.frontend.client.personal.contacts;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.ContactService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of the user's contacts.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ContactsImportPreview extends com.smartgwt.client.widgets.Window {

	private ListGrid list = null;

	public ContactsImportPreview(final ContactsImportSettings settings) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("contactstobeimported"));
		setWidth(500);
		setHeight(550);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		centerInPage();
		setAutoSize(false);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton importButton = new ToolStripButton();
		importButton.setTitle(I18N.message("iimport"));
		toolStrip.addButton(importButton);
		importButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				try {
					ContactService.Instance.get().parseContacts(false, settings.getSeparator(),
							settings.getTextDelimiter(), settings.isSkipFirstRow(), settings.getFirstNameIndex(),
							settings.getLastNameIndex(), settings.getEmailIndex(), settings.getCompanyIndex(),
							settings.getPhoneIndex(), settings.getMobileIndex(), settings.getAddressIndex(),
							new AsyncCallback<GUIContact[]>() {

								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(GUIContact[] contacts) {
									ContactingServer.get().hide();
									settings.destroy();
									destroy();

									try {
										Contacts.get().refresh();
									} catch (Throwable t) {
									}
								}
							});
				} catch (Throwable t) {
					ContactingServer.get().hide();
				}
			}
		});

		ToolStripButton cancel = new ToolStripButton();
		cancel.setTitle(I18N.message("cancel"));
		toolStrip.addButton(cancel);
		cancel.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		toolStrip.addFill();

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField email = new ListGridField("email", I18N.message("email"), 200);
		email.setWidth("*");
		email.setCanFilter(true);

		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"));
		firstName.setCanFilter(true);
		firstName.setWidth(80);

		ListGridField lastName = new ListGridField("lastName", I18N.message("lastname"));
		firstName.setCanFilter(true);
		lastName.setWidth(80);

		ListGridField company = new ListGridField("company", I18N.message("company"));
		company.setCanFilter(true);
		company.setWidth(110);

		ListGridField phone = new ListGridField("phone", I18N.message("phone"));
		phone.setCanFilter(true);
		phone.setWidth(100);
		phone.setHidden(true);

		ListGridField mobile = new ListGridField("mobile", I18N.message("cell"));
		mobile.setCanFilter(true);
		mobile.setWidth(100);
		mobile.setHidden(true);

		ListGridField address = new ListGridField("address", I18N.message("address"));
		address.setCanFilter(true);
		address.setWidth(150);
		address.setHidden(true);

		list = new ListGrid();
		list.setWidth100();
		list.setHeight100();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowFilterEditor(false);
		list.setAutoDraw(true);
		list.setFields(id, email, firstName, lastName, company, phone, mobile, address);

		addItem(toolStrip);
		addItem(list);
	}

	public void setContacts(GUIContact[] contacts) {
		List<ListGridRecord> records = new ArrayList<ListGridRecord>();
		for (GUIContact contact : contacts) {
			ListGridRecord record = new ListGridRecord();
			record.setAttribute("id", contact.getId());
			record.setAttribute("firstName", contact.getFirstName());
			record.setAttribute("lastName", contact.getLastName());
			record.setAttribute("email", contact.getEmail());
			record.setAttribute("company", contact.getCompany());
			record.setAttribute("address", contact.getAddress());
			record.setAttribute("mobile", contact.getMobile());
			record.setAttribute("phone", contact.getPhone());
			records.add(record);
		}
		list.setRecords(records.toArray(new ListGridRecord[0]));
	}
}