package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * Panel for setting up the YubiKey second factor.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class DuoSetup extends TwoFactorsAuthenticationSetup {

	private DynamicForm form;

	public DuoSetup(GUIUser user) {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);

		// The key contains the settings
		String[] settings = new String[] { null, "passcode", "auto" };
		if (user.getKey() != null && user.getKey().contains("|"))
			settings = user.getKey().split("\\|");

		// Prepare the form with account informations
		TextItem username = ItemFactory.newTextItem("username", settings[0]);
		username.setWidth(300);
		username.setRequired(true);

		SelectItem factor = ItemFactory.newSelectItem("factor", I18N.message("factor"));
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("passcode", "passcode");
		map.put("sms", "sms");
		map.put("push", "push");
		factor.setValueMap(map);
		factor.setRequired(true);
		factor.setValue(settings[1]);

		TextItem device = ItemFactory.newTextItem("device", settings[2]);
		device.setWidth(300);

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(2);
		form.setItems(username, factor, device);

		setMembers(form);
	}

	@Override
	public String getKey() {
		validate();
		return super.getKey();
	}

	@Override
	public String getAccount() {
		validate();
		return super.getAccount();
	}

	@Override
	public boolean validate() {
		boolean valid = form.validate();
		if (valid) {
			String username = form.getValueAsString("username");
			String factor = form.getValueAsString("factor");
			String device = form.getValueAsString("device");
			if (device == null || device.isEmpty())
				device = "auto";
			super.key = username + "|" + factor + "|" + device;
			super.account = username;
		}

		return valid;
	}
}