package com.logicaldoc.gui.common.client.validators;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;

/**
 * Typical validator for codes, you can use only alphanumeric chars.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SimpleTextValidator extends RegExpValidator {
	public SimpleTextValidator() {
		super();
		setErrorMessage(I18N.message("simpetextinvalid"));
		setExpression("^([a-zA-Z0-9]+)$");
	}
}