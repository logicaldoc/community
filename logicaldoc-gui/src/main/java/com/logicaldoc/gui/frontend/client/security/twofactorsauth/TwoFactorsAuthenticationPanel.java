package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the settings for the two factors authentication for a
 * specific user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class TwoFactorsAuthenticationPanel extends VLayout {

	private static final String FACTOR = "factor";

	private ValuesManager vm = new ValuesManager();

	private GUIUser user;

	private TwoFactorsAuthenticationSetup setupPanel = new TwoFactorsAuthenticationSetup();

	private CheckboxItem notify;

	public TwoFactorsAuthenticationPanel(GUIUser user, boolean withNotify) {
		super();
		this.user = user;

		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);

		notify = ItemFactory.newCheckbox("notify", "notifyuser");
		notify.setValue(false);
		notify.setDisabled(true);

		SelectItem method = ItemFactory.new2AFMethodSelector(FACTOR, user.getSecondFactor(), true);
		method.addChangedHandler((ChangedEvent event) -> {
			removeMember(setupPanel);
			if (event.getValue() == null || event.getValue().toString().isEmpty()) {
				setupPanel = new TwoFactorsAuthenticationSetup();
				TwoFactorsAuthenticationPanel.this.user.setKey(null);
				TwoFactorsAuthenticationPanel.this.user.setSecondFactor(null);
				notify.setValue(false);
				notify.setDisabled(true);
			} else if (Constants.TWOFA_GOOGLE_AUTHENTICATOR.equals(event.getValue().toString())) {
				setupPanel = new GoogleAuthenticatorSetup(TwoFactorsAuthenticationPanel.this.user);
				notify.setValue(true);
				notify.setDisabled(false);
			} else if (Constants.TWOFA_YUBIKEY.equals(event.getValue().toString())) {
				setupPanel = new YubiKeySetup(TwoFactorsAuthenticationPanel.this.user);
				notify.setValue(false);
				notify.setDisabled(true);
			} else if (Constants.TWOFA_EMAIL_AUTHENTICATOR.equals(event.getValue().toString())) {
				setupPanel = new EmailAuthenticatorSetup();
				notify.setValue(false);
				notify.setDisabled(true);
			} else if (Constants.TWOFA_DUO.equals(event.getValue().toString())) {
				setupPanel = new DuoSetup(TwoFactorsAuthenticationPanel.this.user);
				notify.setValue(false);
				notify.setDisabled(true);
			}
			addMember(setupPanel);
		});

		if (withNotify)
			form.setItems(method, notify);
		else
			form.setItems(method);
		setMembers(form);

		if (Constants.TWOFA_DUO.equals(user.getSecondFactor())) {
			setupPanel = new DuoSetup(TwoFactorsAuthenticationPanel.this.user);
			notify.setValue(false);
			notify.setDisabled(true);
			addMember(setupPanel);
		}
	}

	public String getFactor() {
		return vm.getValueAsString(FACTOR);
	}

	public boolean isNotify() {
		return getFactor() != null && !getFactor().isEmpty() && notify.getValueAsBoolean();
	}

	public String getKey() {
		return setupPanel.getKey();
	}

	public String getAccount() {
		return setupPanel.getAccount();
	}

	public boolean validate() {
		if (Boolean.TRUE.equals(vm.validate()) && setupPanel.validate()) {
			user.setSecondFactor(vm.getValueAsString(FACTOR));
			user.setKey(setupPanel.getKey());
		}

		return vm.validate();
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