package com.logicaldoc.gui.login.client;

import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.gui.login.client.services.LoginServiceAsync;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * Reset the password in case it was lost or forgotten
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class ResetPassword extends Window {

	protected LoginServiceAsync loginService = (LoginServiceAsync) GWT.create(LoginService.class);

	private ValuesManager vm = new ValuesManager();

	private String productName = "";

	public ResetPassword(String product) {
		this.productName = product;
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
		form.setValuesManager(vm);
		TextItem username = ItemFactory.newTextItem("username", "");
		username.setRequired(true);
		TextItem email = ItemFactory.newEmailItem("email", "email", false);
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
		resetButton.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					final String userName = (String) values.get("username");
					final String emailAddress = (String) values.get("email");
					buttonForm.setDisabled(true);
					loginService.resetPassword(userName, emailAddress, productName, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
							buttonForm.setDisabled(false);
						}

						@Override
						public void onSuccess(Void result) {
							buttonForm.setDisabled(false);
							SC.say(I18N.message("amessagewassentto", emailAddress));
							destroy();
						}
					});
				}
			}
		});
		buttonForm.setItems(resetButton);
		addItem(buttonForm);
	}
}