package com.logicaldoc.gui.login.client;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * Reset the password in case it was lost or forgotten
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class ResetPassword extends Window {

	private static final String EMAIL = "email";

	public ResetPassword(String product) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("passwordreset"));
		setWidth(340);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(true);
		setMargin(5);

		DynamicForm form = new DynamicForm();
		form.setMargin(5);
		TextItem username = ItemFactory.newTextItem("username", "");
		username.setRequired(true);
		TextItem email = ItemFactory.newEmailItem(EMAIL, EMAIL, false);
		email.setRequired(true);
		form.setFields(username, email);
		addItem(form);

		Label pwdResetMsg = new Label(I18N.message("passwordresetmessage"));
		pwdResetMsg.setWidth100();
		pwdResetMsg.setMargin(5);
		addItem(pwdResetMsg);

		final DynamicForm buttonForm = new DynamicForm();
		buttonForm.setMargin(5);
		ButtonItem resetButton = new ButtonItem("reset", I18N.message("reset"));
		resetButton.setAutoFit(true);
		resetButton.addClickHandler(click -> {
			if (form.validate()) {
				final String userName = form.getValueAsString("username");
				final String emailAddress = form.getValueAsString(EMAIL);
				buttonForm.setDisabled(true);
				LoginService.Instance.get().resetPassword(userName, emailAddress, product,
						new DefaultAsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								super.onFailure(caught);
								buttonForm.setDisabled(false);
							}

							@Override
							public void handleSuccess(Void result) {
								buttonForm.setDisabled(false);
								SC.say(I18N.message("amessagewassentto", emailAddress));
								destroy();
							}
						});
			}
		});
		buttonForm.setItems(resetButton);
		addItem(buttonForm);
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