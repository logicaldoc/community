package com.logicaldoc.gui.frontend.client.security.user;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.validators.MinLengthValidator;
import com.smartgwt.client.types.AutoComplete;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.validator.MatchesFieldValidator;

/**
 * This is the form used to change the password of a user from the
 * administration interface.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SetPassword extends Window {

	private static final String NOTIFY = "notify";

	private static final String NEWPASSWORDAGAIN = "newpasswordagain";

	private static final String NEWPASSWORD = "newpassword";

	public SetPassword(final long userId) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("changepassword"));
		setWidth(300);
		setHeight(140);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final ValuesManager vm = new ValuesManager();
		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(350);

		MatchesFieldValidator equalsValidator = new MatchesFieldValidator();
		equalsValidator.setOtherField(NEWPASSWORDAGAIN);
		equalsValidator.setErrorMessage(I18N.message("passwordnotmatch"));

		PasswordItem newPass = ItemFactory.newPasswordItemPreventAutocomplete(NEWPASSWORD, NEWPASSWORD, null, true);
		newPass.setRequired(true);
		newPass.setAutoComplete(AutoComplete.NONE);
		newPass.setWrapTitle(false);
		newPass.setValidators(equalsValidator, new MinLengthValidator(Session.get().getUser().getPasswordMinLenght()));

		PasswordItem newPassAgain = ItemFactory.newPasswordItemPreventAutocomplete(NEWPASSWORDAGAIN, NEWPASSWORDAGAIN,
				null);
		newPassAgain.setRequired(true);
		newPassAgain.setAutoComplete(AutoComplete.NONE);
		newPassAgain.setWrapTitle(false);

		CheckboxItem notify = ItemFactory.newCheckbox(NOTIFY, "notifycredentials");
		notify.setValue(false);

		ButtonItem apply = prepareApplyButton(userId, vm, newPass, notify);

		form.setFields(newPass, newPassAgain, notify, apply);

		addItem(form);
	}

	private ButtonItem prepareApplyButton(final long userId, final ValuesManager vm, PasswordItem newPass,
			CheckboxItem notify) {
		ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler((ClickEvent event) -> {
			vm.validate();
			if (Boolean.TRUE.equals(vm.hasErrors()))
				return;

			apply.setDisabled(true);
			SecurityService.Instance.get().changePassword(Session.get().getUser().getId(), userId, null,
					vm.getValueAsString(NEWPASSWORD), notify.getValueAsBoolean(), new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							SC.warn(caught.getMessage());
							apply.setDisabled(false);
						}

						@Override
						public void onSuccess(GUIValue val) {
							apply.setDisabled(false);
							int ret = Integer.parseInt(val.getCode());
							if (ret > 0) {
								// Alert the user
								if (ret == 1)
									GuiLog.warn(I18N.message("wrongpassword"), null);
								else if (ret == 2)
									GuiLog.warn(I18N.message("passwdnotnotified"), null);
								else if (ret == 3) {
									GuiLog.warn(I18N.message("passwdalreadyused", val.getValue()), null);
									newPass.setErrors(I18N.message("passwdalreadyused", val.getValue()));
								} else if (ret == 4) {
									GuiLog.warn(I18N.message("passwdtooweak", val.getValue()), null);
									newPass.setErrors(val.getValue());
								} else
									GuiLog.warn(I18N.message("genericerror"), null);
							} else {
								destroy();
								SC.say(I18N.message("passwordforcedhint"));
							}
						}
					});
		});
		return apply;
	}
}
