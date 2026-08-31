package com.logicaldoc.gui.common.client.validators;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.validator.LengthRangeValidator;

/**
 * A velidator that enforce a minimum length
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 *
 */
public class MinLengthValidator extends LengthRangeValidator {

	public MinLengthValidator(int minLength) {
		setErrorMessage(I18N.message("errorfieldminlenght", Integer.toString(minLength)));
		setMin(minLength);
	}
}