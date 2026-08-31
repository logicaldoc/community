package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;

/**
 * An utility to generate passwords
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class PasswordGenerator extends Window {

	protected DynamicForm form = new DynamicForm();

	protected FormItem password;

	protected ButtonItem submit;

	protected Integer pwdSize;

	protected Integer pwdUpperCase;

	protected Integer pwdLowerCase;

	protected Integer pwdDigit;

	protected Integer pwdSpecial;

	protected Integer pwdSequence;

	protected Integer pwdOccurrence;

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

	protected void onSubmit() {
		submit.setDisabled(true);
		if (pwdSize == null)
			SecurityService.Instance.get().generatePassword(new DefaultAsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					super.onFailure(caught);
					submit.setDisabled(false);
				}

				@Override
				public void onSuccess(String pswd) {
					password.setValue(pswd);
					submit.setDisabled(false);
				}
			});
		else
			SecurityService.Instance.get().generatePassword2(pwdSize, pwdUpperCase, pwdLowerCase, pwdDigit, pwdSpecial,
					pwdSequence, pwdOccurrence, new DefaultAsyncCallback<>() {
						@Override
						public void onFailure(Throwable caught) {
							super.onFailure(caught);
							submit.setDisabled(false);
						}

						@Override
						public void onSuccess(String pswd) {
							password.setValue(pswd);
							submit.setDisabled(false);
						}
					});
	}

	@Override
	protected void onDraw() {
		password = preparePasswordItem();

		submit = new ButtonItem("generate", I18N.message("generate"));
		submit.addClickHandler(event -> {
			if (form.validate())
				onSubmit();
		});

		form.setWidth100();
		form.setHeight(1);
		form.setItems(password, submit);

		addItem(form);
		onSubmit();
	}

	protected FormItem preparePasswordItem() {
		StaticTextItem pswd = new StaticTextItem(I18N.message("password"));
		pswd.setWrapTitle(false);
		pswd.setWrap(false);
		pswd.setIcons(new CopyTextFormItemIcon());
		return pswd;
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