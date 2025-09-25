package com.logicaldoc.gui.frontend.client.tenant;

import java.util.Date;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TenantPropertiesPanel extends HLayout {
	private static final String EMAIL = "email";

	private DynamicForm form = new DynamicForm();

	private GUITenant tenant;

	private ChangedHandler changedHandler;

	private Canvas idLabel;

	private VLayout layout = new VLayout();

	public TenantPropertiesPanel(GUITenant tenant, ChangedHandler changedHandler) {
		if (tenant == null) {
			setMembers(TenantsPanel.SELECT_TENANT);
		} else {
			this.tenant = tenant;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);

			layout.setWidth(300);

			idLabel = new Label(I18N.message("id") + ": " + Long.toString(tenant.getId()));
			idLabel.setHeight(15);
			layout.addMember(idLabel, 0);

			refresh();
		}
	}

	public void refresh() {
		boolean readonly = prepareForm();

		TextItem name = prepareNameItem(readonly);

		TextItem displayName = ItemFactory.newTextItem("displayname", tenant.getDisplayName());
		displayName.setRequired(true);
		displayName.setDisabled(readonly);
		if (!readonly)
			displayName.addChangedHandler(changedHandler);

		TextItem address = ItemFactory.newTextItem("address", tenant.getStreet());
		address.setDisabled(readonly);
		if (!readonly)
			address.addChangedHandler(changedHandler);

		TextItem postalcode = ItemFactory.newTextItem("postalcode", tenant.getPostalCode());
		postalcode.setDisabled(readonly);
		if (!readonly)
			postalcode.addChangedHandler(changedHandler);

		TextItem city = ItemFactory.newTextItem("city", tenant.getCity());
		city.setDisabled(readonly);
		if (!readonly)
			city.addChangedHandler(changedHandler);

		TextItem country = ItemFactory.newTextItem("country", tenant.getCountry());
		country.setDisabled(readonly);
		if (!readonly)
			country.addChangedHandler(changedHandler);

		TextItem state = ItemFactory.newTextItem("state", tenant.getState());
		state.setDisabled(readonly);
		if (!readonly)
			state.addChangedHandler(changedHandler);

		TextItem phone = ItemFactory.newTextItem("phone", tenant.getTelephone());
		phone.setDisabled(readonly);
		if (!readonly)
			phone.addChangedHandler(changedHandler);

		DateItem expire = ItemFactory.newDateItem("expire", "expireson");
		expire.setValue(tenant.getExpire());
		expire.setDisabled(readonly);
		if (!readonly)
			expire.addChangedHandler(changedHandler);

		TextItem email = ItemFactory.newEmailItem(EMAIL, EMAIL, false);
		email.setDisabled(readonly);
		email.setValue(tenant.getEmail());
		if (!readonly)
			email.addChangedHandler(changedHandler);

		if (readonly || tenant.isDefault()) {
			expire.setDisabled(true);
		} else {
			expire.addChangedHandler(changedHandler);
		}

		form.setItems(name, displayName, expire, email, address, postalcode, city, country, state, phone);
		addMember(layout);
	}

	private TextItem prepareNameItem(boolean readonly) {
		TextItem name = ItemFactory.newSimpleTextItem("name", tenant.getName());
		name.setRequired(true);
		name.setSelectOnFocus(true);
		name.setDisabled(readonly || (tenant.getId() != 0 && Constants.TENANT_DEFAULTNAME.equals(tenant.getName())));
		if (!readonly)
			name.addChangedHandler(changedHandler);
		return name;
	}

	private boolean prepareForm() {
		boolean readonly = (changedHandler == null);
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(contains(form)))
			removeChild(form);

		form = new DynamicForm();
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(3);

		layout.addMember(form, 1);
		return readonly;
	}

	boolean validate() {
		if (form.validate()) {
			if (form.getValue("name") != null)
				tenant.setName(form.getValueAsString("name"));

			tenant.setDisplayName(form.getValueAsString("displayname"));
			tenant.setStreet(form.getValueAsString("address"));
			tenant.setCity(form.getValueAsString("city"));
			tenant.setCountry(form.getValueAsString("country"));
			tenant.setState(form.getValueAsString("state"));
			tenant.setPostalCode(form.getValueAsString("postalcode"));
			tenant.setTelephone(form.getValueAsString("phone"));
			tenant.setEmail(form.getValueAsString(EMAIL));
			tenant.setEnabled(Boolean.valueOf(form.getValueAsString("eenabled")));
			tenant.setExpire((Date) form.getValue("expire"));
		}

		return !form.hasErrors();
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