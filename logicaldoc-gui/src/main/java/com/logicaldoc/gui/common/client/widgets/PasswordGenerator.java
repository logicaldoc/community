package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
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

	private Integer pwdSize;

	private Integer pwdUpperCase;

	private Integer pwdLowerCase;

	private Integer pwdDigit;

	private Integer pwdSpecial;

	private Integer pwdSequence;

	private Integer pwdOccurrence;

	public PasswordGenerator(Integer pwdSize, Integer pwdUpperCase, Integer pwdLowerCase, Integer pwdDigit,
			Integer pwdSpecial, Integer pwdSequence, Integer pwdOccurrence) {
		this();
		this.pwdSize = pwdSize;
		this.pwdUpperCase = pwdUpperCase;
		this.pwdLowerCase = pwdLowerCase;
		this.pwdDigit = pwdDigit;
		this.pwdSpecial = pwdSpecial;
		this.pwdSequence = pwdSequence;
		this.pwdOccurrence = pwdOccurrence;
	}

	public PasswordGenerator() {
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
		if (pwdSize == null)
			SecurityService.Instance.get().generatePassword(new AsyncCallback<String>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
					generate.setDisabled(false);
				}

				@Override
				public void onSuccess(String pswd) {
					password.setValue(pswd);
					generate.setDisabled(false);
				}
			});
		else
			SecurityService.Instance.get().generatePassword2(pwdSize, pwdUpperCase, pwdLowerCase, pwdDigit, pwdSpecial,
					pwdSequence, pwdOccurrence, new AsyncCallback<String>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
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