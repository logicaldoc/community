package com.logicaldoc.gui.login.client;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.gui.login.client.services.LoginServiceAsync;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
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

	private LoginServiceAsync loginService = (LoginServiceAsync) GWT.create(LoginService.class);

	public ChangePassword(final GUIUser user, final LoginPanel loginPanel) {
		super();

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
		form.setWidth(300);
		form.setMargin(5);

		PasswordItem password = ItemFactory.newPasswordItemPreventAutocomplete(PASSWORD, "currentpassword", null);
		password.setRequired(true);

		MatchesFieldValidator equalsValidator = new MatchesFieldValidator();
		equalsValidator.setOtherField(NEWPASSWORDAGAIN);
		equalsValidator.setErrorMessage(I18N.message("passwordnotmatch"));

		LengthRangeValidator sizeValidator = new LengthRangeValidator();
		sizeValidator
				.setErrorMessage(I18N.message("errorfieldminlenght", Integer.toString(user.getPasswordMinLenght())));
		sizeValidator.setMin(user.getPasswordMinLenght());

		PasswordItem newPass = ItemFactory.newPasswordItemPreventAutocomplete(NEWPASSWORD, NEWPASSWORD, null);
		newPass.setRequired(true);
		newPass.setValidators(equalsValidator, sizeValidator);

		FormItemIcon generator = new FormItemIcon();
		generator.setName("generator");
		generator.setWidth(16);
		generator.setHeight(16);

		generator.setSrc("[SKIN]/key.png");
		generator.setPrompt(I18N.message("passwordgenerator"));
		generator.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			PasswordGenerator pswGenerator = new PasswordGenerator(user.getUsername());
			pswGenerator.show();
		});
		newPass.setIcons(generator);
		newPass.setIconVAlign(VerticalAlignment.CENTER);

		PasswordItem newPassAgain = ItemFactory.newPasswordItemPreventAutocomplete(NEWPASSWORDAGAIN, NEWPASSWORDAGAIN,
				null);
		newPassAgain.setWrapTitle(false);
		newPassAgain.setRequired(true);

		ButtonItem apply = prepareApplyButton(user, loginPanel, newPass, vm);

		form.setFields(password, newPass, newPassAgain, apply);

		Label label = new Label();
		label.setHeight(30);
		label.setPadding(10);
		label.setMargin(5);
		label.setAlign(Alignment.CENTER);
		label.setValign(VerticalAlignment.CENTER);
		label.setWrap(false);
		label.setIcon("[SKIN]/Dialog/warn.png");
		label.setContents(I18N.message("needtochangepassword"));
		addItem(label);

		addItem(form);
	}

	private ButtonItem prepareApplyButton(final GUIUser user, LoginPanel loginPanel, PasswordItem newPass,
			ValuesManager vm) {
		ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler((ClickEvent event) -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			if (vm.getValueAsString(PASSWORD).equals(vm.getValueAsString(NEWPASSWORD))) {
				Map<String, String> errors = new HashMap<>();
				errors.put(NEWPASSWORD, I18N.message("useanotherpassword"));
				vm.setErrors(errors, true);
				return;
			}

			doChangePassword(loginPanel, user.getId(), newPass, vm);
		});
		return apply;
	}

	private void doChangePassword(LoginPanel loginPanel, long userId, PasswordItem newPass, ValuesManager vm) {
		loginService.changePassword(userId, vm.getValueAsString(PASSWORD), vm.getValueAsString(NEWPASSWORD),
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						SC.warn(caught.getMessage());
					}

					@Override
					public void onSuccess(GUIValue val) {
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
							// Close the popup
							ChangePassword.this.destroy();
							SC.say(I18N.message("yourpasswordhaschanged"));
							GuiLog.info(I18N.message("event.user.passwordchanged"), null);
							if (loginPanel != null)
								loginPanel.onPasswordChanged();
						}
					}
				});
	}
}
