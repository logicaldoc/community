package com.logicaldoc.gui.common.client.validators;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;

/**
 * Validates a single e-mail address
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailValidator extends RegExpValidator {

	public EmailValidator() {
		super();
		setErrorMessage(I18N.message("invalidemail"));
		setExpression("([a-zA-Z0-9_.\\-+])+@(([a-zA-Z0-9\\-])+\\.)+([a-zA-Z0-9\\-])+");
	}
}
