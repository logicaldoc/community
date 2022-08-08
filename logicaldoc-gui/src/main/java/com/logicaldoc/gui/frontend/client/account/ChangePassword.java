package com.logicaldoc.gui.frontend.client.account;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;
import com.smartgwt.client.widgets.form.validator.MatchesFieldValidator;

/**
 * This is the form used to change the password of the current user, for example
 * when the password expired.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ChangePassword extends Window {
	private static final String PASSWORD = "password";

	private static final String NEWPASSWORDAGAIN = "newpasswordagain";

	private static final String NEWPASSWORD = "newpassword";

	public ChangePassword() {
		super();

		GUIUser user = Session.get().getUser();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("changepassword"));
		setWidth(300);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final ValuesManager vm = new ValuesManager();
		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(350);
		form.setMargin(5);

		PasswordItem password = ItemFactory.newPasswordItemPreventAutocomplete(PASSWORD, PASSWORD, null);
		password.setRequired(true);

		MatchesFieldValidator equalsValidator = new MatchesFieldValidator();
		equalsValidator.setOtherField(NEWPASSWORDAGAIN);
		equalsValidator.setErrorMessage(I18N.message("passwordnotmatch"));

		LengthRangeValidator sizeValidator = new LengthRangeValidator();
		sizeValidator
				.setErrorMessage(I18N.message("errorfieldminlenght", Integer.toString(user.getPasswordMinLenght())));
		sizeValidator.setMin(user.getPasswordMinLenght());

		PasswordItem newPass = ItemFactory.newPasswordItemPreventAutocomplete(NEWPASSWORD, NEWPASSWORD, null, true);
		newPass.setWrapTitle(false);
		newPass.setRequired(true);
		newPass.setValidators(equalsValidator, sizeValidator);

		PasswordItem newPassAgain = ItemFactory.newPasswordItemPreventAutocomplete(NEWPASSWORDAGAIN, NEWPASSWORDAGAIN,
				null);
		newPassAgain.setWrapTitle(false);
		newPassAgain.setRequired(true);

		final ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				vm.validate();
				if (!vm.hasErrors()) {
					if (vm.getValueAsString(PASSWORD).equals(vm.getValueAsString(NEWPASSWORD))) {
						Map<String, String> errors = new HashMap<String, String>();
						errors.put(NEWPASSWORD, I18N.message("useanotherpassword"));
						vm.setErrors(errors, true);
						return;
					}

					apply.setDisabled(true);

					SecurityService.Instance.get().changePassword(user.getId(), user.getId(),
							vm.getValueAsString(PASSWORD), vm.getValueAsString(NEWPASSWORD), false,
							new AsyncCallback<GUIValue>() {

								@Override
								public void onFailure(Throwable caught) {
									apply.setDisabled(false);
									SC.warn(caught.getMessage());
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
										SC.say(I18N.message("yourpasswordhaschanged"));
										GuiLog.info(I18N.message("event.user.passwordchanged"), null);
										ChangePassword.this.destroy();
									}
								}
							});
				}
			}
		});

		form.setFields(password, newPass, newPassAgain, apply);

		addItem(form);
	}
}
