package com.logicaldoc.core.automation;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import com.logicaldoc.i18n.I18N;

/**
 * Utility class to handle translations from inside Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
@AutomationDictionary
public class I18NTool extends HashMap<String, String> {

	private static final long serialVersionUID = 1L;

	public I18NTool() {
		this(I18N.getMessages(Locale.ENGLISH));
	}
	
	public I18NTool(Map<String, String> m) {
		super(m);
	}

	/**
	 * Prints a formatted string
	 * 
	 * @param key key of the string in the localization bundle
	 * @param value is replaced into the {0} place holder
	 * 
	 * @return the formatted string
	 */
	public String format(String key, String value) {
		return format(key, new String[] { value });
	}

	/**
	 * Prints a formatted string
	 * 
	 * @param key key of the string in the localization bundle
	 * @param value1 is replaced into the {0} place holder
	 * @param value2 is replaced into the {1} place holder
	 * 
	 * @return the formatted string
	 */
	public String format(String key, String value1, String value2) {
		return format(key, new String[] { value1, value2 });
	}

	/**
	 * Prints a formatted string
	 * 
	 * @param key key of the string in the localization bundle
	 * @param value1 is replaced into the {0} place holder
	 * @param value2 is replaced into the {1} place holder
	 * @param value3 is replaced into the {2} place holder
	 * 
	 * @return the formatted string
	 */
	public String format(String key, String value1, String value2, String value3) {
		return format(key, new String[] { value1, value2, value3 });
	}

	/**
	 * Prints a formatted string
	 * 
	 * @param key key of the string in the localization bundle
	 * @param value1 is replaced into the {0} place holder
	 * @param value2 is replaced into the {1} place holder
	 * @param value3 is replaced into the {2} place holder
	 * @param value4 is replaced into the {3} place holder
	 * 
	 * @return the formatted string
	 */
	public String format(String key, String value1, String value2, String value3, String value4) {
		return format(key, new String[] { value1, value2, value3, value4 });
	}

	/**
	 * Prints a formatted string
	 * 
	 * @param key key of the string in the localization bundle
	 * @param value1 is replaced into the {0} place holder
	 * @param value2 is replaced into the {1} place holder
	 * @param value3 is replaced into the {2} place holder
	 * @param value4 is replaced into the {3} place holder
	 * @param value5 is replaced into the {4} place holder
	 * 
	 * @return the formatted string
	 */
	public String format(String key, String value1, String value2, String value3, String value4, String value5) {
		return format(key, new String[] { value1, value2, value3, value4, value5 });
	}

	/**
	 * Prints a formatted string
	 * 
	 * @param key key of the string in the localization bundle
	 * @param values used to fill the place holders <code>{X}</code> will be replaced with <code>values[X-1]</code>
	 * 
	 * @return the formatted string
	 */
	public String format(String key, String[] values) {
		return MessageFormat.format(get(key), (Object[]) values);
	}


	/**
	 * Gets the not formatted string
	 * 
	 * @param key the key to retrieve
	 * 
	 * @return the not formatted string
	 */
	@Override
	public String get(Object key) {
		return super.getOrDefault(key, key.toString());
	}
}