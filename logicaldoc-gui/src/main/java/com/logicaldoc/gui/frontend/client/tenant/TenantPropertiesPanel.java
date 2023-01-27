package com.logicaldoc.gui.frontend.client.tenant;

import java.util.Date;
import java.util.Map;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
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
	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

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

		CheckboxItem enabled = new CheckboxItem("eenabled", I18N.message("enabled"));
		enabled.setValue(tenant.isEnabled());

		TextItem name = prepareNameItem(readonly);

		TextItem displayName = ItemFactory.newTextItem("displayname", "displayname", tenant.getDisplayName());
		displayName.setRequired(true);
		displayName.setDisabled(readonly);
		if (!readonly)
			displayName.addChangedHandler(changedHandler);

		TextItem address = ItemFactory.newTextItem("address", "address", tenant.getStreet());
		address.setDisabled(readonly);
		if (!readonly)
			address.addChangedHandler(changedHandler);

		TextItem postalcode = ItemFactory.newTextItem("postalcode", "postalcode", tenant.getPostalCode());
		postalcode.setDisabled(readonly);
		if (!readonly)
			postalcode.addChangedHandler(changedHandler);

		TextItem city = ItemFactory.newTextItem("city", "city", tenant.getCity());
		city.setDisabled(readonly);
		if (!readonly)
			city.addChangedHandler(changedHandler);

		TextItem country = ItemFactory.newTextItem("country", "country", tenant.getCountry());
		country.setDisabled(readonly);
		if (!readonly)
			country.addChangedHandler(changedHandler);

		TextItem state = ItemFactory.newTextItem("state", "state", tenant.getState());
		state.setDisabled(readonly);
		if (!readonly)
			state.addChangedHandler(changedHandler);

		TextItem phone = ItemFactory.newTextItem("phone", "phone", tenant.getTelephone());
		phone.setDisabled(readonly);
		if (!readonly)
			phone.addChangedHandler(changedHandler);

		DateItem expire = ItemFactory.newDateItem("expire", "expireson");
		expire.setValue(tenant.getExpire());
		expire.setDisabled(readonly);
		if (!readonly)
			expire.addChangedHandler(changedHandler);

		TextItem email = ItemFactory.newEmailItem("email", "email", false);
		email.setDisabled(readonly);
		email.setValue(tenant.getEmail());
		if (!readonly)
			email.addChangedHandler(changedHandler);

		if (readonly || tenant.isDefault()) {
			enabled.setDisabled(true);
			expire.setDisabled(true);
		} else {
			enabled.addChangedHandler(changedHandler);
			expire.addChangedHandler(changedHandler);
		}

		form.setItems(name, enabled, expire, displayName, email, address, postalcode, city, country, state, phone);
		addMember(layout);
	}

	private TextItem prepareNameItem(boolean readonly) {
		TextItem name = ItemFactory.newSimpleTextItem("name", "name", tenant.getName());
		name.setRequired(true);
		name.setSelectOnFocus(true);
		name.setDisabled(readonly || (tenant.getId() != 0 && Constants.TENANT_DEFAULTNAME.equals(tenant.getName())));
		if (!readonly)
			name.addChangedHandler(changedHandler);
		return name;
	}

	private boolean prepareForm() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);

		if (form != null)
			form.destroy();

		if (contains(form))
			removeChild(form);

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(3);

		layout.addMember(form, 1);
		return readonly;
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			if (values.get("name") != null)
				tenant.setName((String) values.get("name"));

			tenant.setDisplayName((String) values.get("displayname"));
			tenant.setStreet((String) values.get("address"));
			tenant.setCity((String) values.get("city"));
			tenant.setCountry((String) values.get("country"));
			tenant.setState((String) values.get("state"));
			tenant.setPostalCode((String) values.get("postalcode"));
			tenant.setTelephone((String) values.get("phone"));
			tenant.setEmail((String) values.get("email"));
			tenant.setEnabled(Boolean.valueOf(values.get("eenabled").toString()));
			tenant.setExpire((Date) values.get("expire"));
		}

		return !vm.hasErrors();
	}
}