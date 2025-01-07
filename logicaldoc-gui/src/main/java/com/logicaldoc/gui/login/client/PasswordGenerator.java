package com.logicaldoc.gui.login.client;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.CopyTextFormItemIcon;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;

/**
 * An utility to generate passwords
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class PasswordGenerator extends Window {

	private DynamicForm form = new DynamicForm();

	private StaticTextItem password;

	private ButtonItem generate;

	private String username;

	public PasswordGenerator(String username) {
		this.username = username;
		setTitle(I18N.message("passwordgenerator"));
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
	}

	private void generatePassword() {
		generate.setDisabled(true);
		LoginService.Instance.get().generatePassword(username, new DefaultAsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				super.onFailure(caught);
				generate.setDisabled(false);
			}

			@Override
			public void onSuccess(String pswd) {
				password.setValue(pswd);
				generate.setDisabled(false);
			}
		});
	}

	@Override
	protected void onDraw() {
		password = new StaticTextItem(I18N.message("password"));
		password.setWrapTitle(false);
		password.setWrap(false);
		password.setIcons(new CopyTextFormItemIcon());

		generate = new ButtonItem("generate", I18N.message("generate"));
		generate.addClickHandler(event -> {
			if (form.validate())
				generatePassword();
		});

		form.setWidth(1);
		form.setHeight(1);
		form.setItems(password, generate);

		addItem(form);
		generatePassword();
	}
}