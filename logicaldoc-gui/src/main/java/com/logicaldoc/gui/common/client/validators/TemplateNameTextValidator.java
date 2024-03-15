package com.logicaldoc.gui.common.client.validators;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;

/**
 * Tipical validator for tempalte names
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public class TemplateNameTextValidator extends RegExpValidator {
	public TemplateNameTextValidator() {
		super();
		setErrorMessage(I18N.message("tempaltenameinvalid"));
		setExpression("^([a-zA-Z0-9]+)$");
	}
}