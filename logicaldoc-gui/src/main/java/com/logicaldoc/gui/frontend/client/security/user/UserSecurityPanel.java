package com.logicaldoc.gui.frontend.client.security.user;

import java.util.Date;
import java.util.Map;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows user's security settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class UserSecurityPanel extends VLayout {
	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIUser user;

	private ChangedHandler changedHandler;

	public UserSecurityPanel(GUIUser user, ChangedHandler changedHandler) {
		if (user == null) {
			setMembers(UsersPanel.SELECT_USER);
		} else {
			this.user = user;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);

			prepareGUI();
		}
	}

	public void prepareGUI() {
		setAlign(Alignment.LEFT);

		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);

		if (form != null)
			form.destroy();

		if (contains(form))
			removeChild(form);

		form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		CheckboxItem passwordExpires = new CheckboxItem("passwordExpires", I18N.message("passwordexpires"));
		passwordExpires.setValue(user.isPasswordExpires());
		passwordExpires.setDisabled(readonly || (Session.get().isDemo() && Session.get().getUser().getId() == 1));
		if (!readonly)
			passwordExpires.addChangedHandler(changedHandler);

		CheckboxItem enabled = new CheckboxItem("eenabled", I18N.message("enabled"));
		enabled.setValue(user.isEnabled());
		if (readonly || "admin".equals(user.getUsername())) {
			enabled.setDisabled(true);
		} else {
			enabled.addChangedHandler(changedHandler);
		}

		CheckboxItem enforceWorkingTime = new CheckboxItem("enforceworkingtime", I18N.message("enforceworkingtime"));
		enforceWorkingTime.setValue(user.isEnforceWorkingTime());
		if (!readonly)
			enforceWorkingTime.addChangedHandler(changedHandler);

		DateItem expire = ItemFactory.newDateItem("expireson", "expireson");
		expire.setValue(user.getExpire());
		if (readonly || "admin".equals(user.getUsername())) {
			expire.setDisabled(true);
		} else {
			expire.addChangedHandler(changedHandler);
			expire.addKeyPressHandler(new KeyPressHandler() {
				@Override
				public void onKeyPress(KeyPressEvent event) {
					if ("backspace".equals(event.getKeyName().toLowerCase())
							|| "delete".equals(event.getKeyName().toLowerCase())) {
						expire.clearValue();
						expire.setValue((Date) null);
						changedHandler.onChanged(null);
					} else {
						changedHandler.onChanged(null);
					}
				}
			});
		}

		SpinnerItem maxInactivity = ItemFactory.newSpinnerItem("maxinactivity", "maxinactivity",
				user.getMaxInactivity());
		maxInactivity.setRequired(false);
		maxInactivity.setHint(I18N.message("days"));
		maxInactivity.setWidth(50);
		maxInactivity.setStep(1);
		maxInactivity.setMin(-1);
		if (readonly || "admin".equals(user.getUsername())) {
			maxInactivity.setDisabled(true);
		} else {
			maxInactivity.addChangedHandler(changedHandler);
			maxInactivity.addKeyPressHandler(new KeyPressHandler() {
				@Override
				public void onKeyPress(KeyPressEvent event) {
					if ("backspace".equals(event.getKeyName().toLowerCase())
							|| "delete".equals(event.getKeyName().toLowerCase())) {
						maxInactivity.clearValue();
						maxInactivity.setValue((Date) null);
						changedHandler.onChanged(null);
					} else {
						changedHandler.onChanged(null);
					}
				}
			});
		}

		SelectItem tfa = ItemFactory.new2AFMethodSelector("2fa", user.getSecondFactor(), false);
		tfa.setTitle(I18N.message("twofactorsauth"));
		tfa.setDisabled(readonly || (Session.get().isDemo()));
		if (!readonly)
			tfa.addChangedHandler(changedHandler);
		
		form.setItems(enabled, passwordExpires, enforceWorkingTime, maxInactivity, expire, tfa);

		addMember(form);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			user.setPasswordExpires(Boolean.parseBoolean(values.get("passwordExpires").toString()));
			user.setEnabled(Boolean.parseBoolean(values.get("eenabled").toString()));
			user.setEnforceWorkingTime(Boolean.parseBoolean(values.get("enforceworkingtime").toString()));
			user.setMaxInactivity((Integer) values.get("maxinactivity"));
			user.setExpire((Date) values.get("expireson"));
			user.setSecondFactor(values.get("2fa").toString());
		}
		return !vm.hasErrors();
	}
}