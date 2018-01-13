package com.logicaldoc.core.automation;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

/**
 * Utility class to handle dates from inside Velocity macros
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class I18NTool extends HashMap<String, String> {

	private static final long serialVersionUID = 1L;

	public I18NTool(Map<? extends String, ? extends String> m) {
		super(m);
	}

	public String format(String key, String value) {
		return format(key, new String[] { value });
	}

	public String format(String key, String value1, String value2) {
		return format(key, new String[] { value1, value2 });
	}

	public String format(String key, String value1, String value2, String value3) {
		return format(key, new String[] { value1, value2, value3 });
	}

	public String format(String key, String value1, String value2, String value3, String value4) {
		return format(key, new String[] { value1, value2, value3, value4 });
	}

	public String format(String key, String value1, String value2, String value3, String value4, String value5) {
		return format(key, new String[] { value1, value2, value3, value4, value5 });
	}

	public String format(String key, String[] values) {
		return MessageFormat.format(get(key), (Object[]) values);
	}

	@Override
	public String get(Object key) {
		return super.getOrDefault(key, key.toString());
	}
}