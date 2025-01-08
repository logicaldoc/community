package com.logicaldoc.gui.frontend.client.security;

import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.PasswordGenerator;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * An utility to allow an administrator to check if a password would pass the
 * quality rules.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class PasswordTrial extends PasswordGenerator {

	private static final String PASSWORD_CONSTANT = "password";
	
	private StaticTextItem result = new StaticTextItem("");

	public PasswordTrial(Integer pwdSize, Integer pwdUpperCase, Integer pwdLowerCase, Integer pwdDigit,
			Integer pwdSpecial, Integer pwdSequence, Integer pwdOccurrence) {
		super(pwdSize, pwdUpperCase, pwdLowerCase, pwdDigit, pwdSpecial, pwdSequence, pwdOccurrence);
		setTitle(I18N.message("tryapassword"));
	}

	public PasswordTrial() {
		setTitle(I18N.message("tryapassword"));
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		super.onDraw();
		submit.setTitle(I18N.message("validate"));
		result.setShowTitle(false);
		result.setColSpan(2);

		form.setItems(password, result, submit);
	}

	@Override
	protected void onSubmit() {
		if (form.getValueAsString(PASSWORD_CONSTANT) == null || form.getValueAsString(PASSWORD_CONSTANT).isEmpty())
			return;

		if(!form.validate())
			return;
		
		submit.setDisabled(true);
		SecurityService.Instance.get().validatePassword(form.getValueAsString(PASSWORD_CONSTANT), pwdSize, pwdUpperCase,
				pwdLowerCase, pwdDigit, pwdSpecial, pwdSequence, pwdOccurrence, new DefaultAsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						submit.setDisabled(false);
					}

					@Override
					public void onSuccess(List<String> failures) {
						if (failures.isEmpty()) {
							result.setValue("<p style='color:green'>" + I18N.message("thispasswdisvalid") + "</p>");
						} else {
							result.setValue("<ol>"+failures.stream().map(failure->"<li>"+failure+"</li>").collect(Collectors.joining())+"</ol>");
						}

						submit.setDisabled(false);
					}
				});
	}

	@Override
	protected FormItem preparePasswordItem() {
		TextItem password = ItemFactory.newTextItem(PASSWORD_CONSTANT, null);
		password.setWidth(200);
		password.setWrapTitle(false);
		password.setRequired(true);
		return password;
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