package com.logicaldoc.core.automation;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.util.LocaleUtil;

/**
 * Extension of the standard NumberTool that allows the locale specification as
 * string.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
@AutomationDictionary(key = "NumberTool")
public class AutomationNumberTool extends org.apache.velocity.tools.generic.NumberTool {

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

	/**
	 * Converts a string into a Long
	 * 
	 * @param str The string to convert
	 * 
	 * @return The converted value
	 */
	public Long toLong(String str) {
		if (StringUtils.isNotEmpty(str))
			return Long.parseLong(str);
		else
			return null;
	}
}