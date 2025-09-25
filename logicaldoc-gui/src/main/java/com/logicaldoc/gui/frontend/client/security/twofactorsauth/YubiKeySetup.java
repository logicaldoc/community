package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * Panel for setting up the YubiKey second factor.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class YubiKeySetup extends TwoFactorsAuthenticationSetup {

	private static final String SECRETKEY = "secretkey";

	private DynamicForm form;

	public YubiKeySetup(GUIUser user) {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);

		// The key contains the settings
		String[] settings = new String[] { null, null };
		if (user.getKey() != null && user.getKey().contains("|"))
			settings = user.getKey().split("\\|");

		// Prepare the form with account informations
		TextItem clientId = ItemFactory.newTextItem("clientid", settings[0]);
		clientId.setWidth(80);
		clientId.setRequired(true);

		TextItem secretKey = ItemFactory.newPasswordItem(SECRETKEY, SECRETKEY, settings[1]);
		secretKey.setWidth(220);
		secretKey.setStartRow(true);
		secretKey.setRequired(true);

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		form.setItems(clientId, secretKey);

		Label hint = new Label(I18N.message(Constants.TWOFA_YUBIKEY + ".hint1"));
		hint.setAutoHeight();

		setMembers(hint, form);
	}

	@Override
	public boolean validate() {
		boolean valid = form.validate();
		if (valid)
			super.key = form.getValueAsString("clientid") + "|" + form.getValueAsString(SECRETKEY);
		return valid;
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