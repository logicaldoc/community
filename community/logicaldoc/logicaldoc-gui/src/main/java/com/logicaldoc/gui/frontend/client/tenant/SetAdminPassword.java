package com.logicaldoc.gui.frontend.client.tenant;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.services.TenantService;
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
 * This is the form used to change the password of the tenant's administrator.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class SetAdminPassword extends Window {

	private static final String NEWPASSWORD = "newpassword";

	private static final String NEWPASSWORDAGAIN = "newpasswordagain";

	public SetAdminPassword(final String tenantName) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("changepassword"));
		setWidth(300);
		setHeight(125);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMembersMargin(5);
		setAutoSize(true);

		final ValuesManager vm = new ValuesManager();
		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(350);
		form.setMargin(5);

		MatchesFieldValidator equalsValidator = new MatchesFieldValidator();
		equalsValidator.setOtherField(NEWPASSWORDAGAIN);
		equalsValidator.setErrorMessage(I18N.message("passwordnotmatch"));

		LengthRangeValidator sizeValidator = new LengthRangeValidator();
		sizeValidator.setErrorMessage(I18N.message("errorfieldminlenght",
				Integer.toString(Session.get().getUser().getPasswordMinLenght())));
		sizeValidator.setMin(Session.get().getUser().getPasswordMinLenght());

		PasswordItem newPass = new PasswordItem();
		newPass.setName(NEWPASSWORD);
		newPass.setTitle(I18N.message(NEWPASSWORD));
		newPass.setRequired(true);
		newPass.setValidators(equalsValidator, sizeValidator);

		PasswordItem newPassAgain = new PasswordItem();
		newPassAgain.setName(NEWPASSWORDAGAIN);
		newPassAgain.setTitle(I18N.message(NEWPASSWORDAGAIN));
		newPassAgain.setWrapTitle(false);
		newPassAgain.setRequired(true);

		final ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				vm.validate();
				if (!vm.hasErrors()) {
					apply.setDisabled(true);
					TenantService.Instance.get().changeAdminPassword(vm.getValueAsString(NEWPASSWORD).trim(),
							tenantName, new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									SC.warn(caught.getMessage());
									apply.setDisabled(false);
								}

								@Override
								public void onSuccess(Void arg) {
									apply.setDisabled(false);
									SetAdminPassword.this.destroy();
									Log.info(I18N.message("event.user.passwordchanged"), null);
								}
							});
				}
			}
		});

		form.setFields(newPass, newPassAgain, apply);
		addItem(form);
	}
}
