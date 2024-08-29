package com.logicaldoc.core.automation;

import org.apache.commons.lang3.StringUtils;

/**
 * Utility methods to handle strings
 *
 * @since 9.0
 */
@AutomationDictionary
public class StringTool {

	/**
	 * Returns either the passed in String, or if the String is null, an empty
	 * String ("").
	 * 
	 * @param str the String to check, may be null
	 * 
	 * @return the passed in String, or the empty String if it was null
	 */
	public String defaultString(String str) {
		return StringUtils.defaultString(str);
	}

	/**
	 * Returns either the passed in String, or if the String is null, the value
	 * of defaultStr.
	 * 
	 * @param str the String to check, may be null
	 * @param defaultStr the default string to return if the input is empty ("")
	 *        or null, may be null
	 * 
	 * @return the passed in String, or the empty String if it was null
	 */
	public String defaultString(String str, String defaultStr) {
		return StringUtils.defaultIfEmpty(str, defaultStr);
	}
}