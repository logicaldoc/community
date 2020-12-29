package com.logicaldoc.core.automation;

import com.logicaldoc.util.LocaleUtil;

/**
 * Extension of the standard NumberTool that allows the locale specification as
 * string.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
@AutomationDictionary
public class NumberTool extends org.apache.velocity.tools.generic.NumberTool {

	private static final long serialVersionUID = 1L;

	/**
	 * Formats a number using the specified format
	 * 
	 * @param format the format to use
	 * @param obj the number to format
	 * @param locale a locale specification
	 * 
	 * @return the formatted number
	 */
	public String format(String format, Object obj, String locale) {
		return super.format(format, obj, LocaleUtil.toLocale(locale));
	}
}