package com.logicaldoc.i18n;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

/**
 * A class for retrieval of localized messages. All bundles declared in
 * ResourceBundle extension point. The first key match wins.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class I18N {
	private I18N() {
	}

	public static String message(String key) {
		return message(key, Locale.getDefault());
	}

	public static String message(String key, String lang) {
		if(lang==null)
			lang="en";
		return message(key, new Locale(lang));
	}

	public static String message(String key, Locale locale) {
		try {
			ResourceBundle bundle = ResourceBundle.getBundle("i18n.messages", locale);
			return bundle.getString(key);
		} catch (Throwable t) {
		}

		return key;
	}

	public static String message(String key, Locale locale, String value) {
		return message(key, locale, new Object[] { value });
	}

	public static String message(String key, Locale locale, Object[] values) {
		String msg = message(key, locale);
		return MessageFormat.format(msg, values);
	}

	public static List<String> getLocales() {
		List<String> locales = new ArrayList<String>();
		Properties p = new Properties();
		try {
			p.load(I18N.class.getResourceAsStream("/i18n/i18n.properties"));
			StringTokenizer st = new StringTokenizer(p.getProperty("locales"), ",", false);
			while (st.hasMoreTokens())
				locales.add(st.nextToken());
		} catch (IOException e) {
			e.printStackTrace();
		}
		return locales;
	}

	public static Map<String, String> getMessages(Locale locale) {
		Map<String, String> map = new HashMap<String, String>();
		try {
			ResourceBundle bundle = ResourceBundle.getBundle("i18n.messages", locale);
			Enumeration<String> keys = bundle.getKeys();
			while (keys.hasMoreElements()) {
				String key = keys.nextElement();
				map.put(key, bundle.getString(key));
			}
		} catch (Throwable t) {
		}
		return map;
	}
}