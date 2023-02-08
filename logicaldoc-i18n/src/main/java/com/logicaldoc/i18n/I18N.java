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
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class I18N {

	/**
	 * Index is the language ISO 639-2(3 digits) value is the corresponding
	 * locale.
	 */
	private static Map<String, Locale> iso3LocaleMap;

	public static String[] bundles = new String[] { "i18n.messages" };

	static {
		String[] languages = Locale.getISOLanguages();
		iso3LocaleMap = new HashMap<>(languages.length);
		for (String language : languages) {
			Locale locale = new Locale(language);
			iso3LocaleMap.put(locale.getISO3Language().toLowerCase(), locale);
		}
	}

	private I18N() {
	}

	public static String message(String key) {
		return message(key, Locale.getDefault());
	}

	public static String message(String key, String lang) {
		if (lang == null)
			lang = "en";
		return message(key, new Locale(lang));
	}

	public static String message(String key, Locale locale) {
		for (String b : bundles) {
			try {
				ResourceBundle bundle = ResourceBundle.getBundle(b, locale);
				if (!bundle.containsKey(key))
					continue;
				String val = bundle.getString(key);
				if (val != null && !val.isEmpty())
					return val;
			} catch (Throwable t) {
				// Nothing to do
			}
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

	public static Map<String, String> getMessages(Locale locale) {
		Map<String, String> map = new HashMap<>();

		for (String b : bundles) {
			try {
				ResourceBundle bundle = ResourceBundle.getBundle(b, locale);
				Enumeration<String> keys = bundle.getKeys();
				while (keys.hasMoreElements()) {
					String key = keys.nextElement();
					if (!bundle.containsKey(key))
						continue;
					String val = bundle.getString(key);
					if (val != null && !val.isEmpty())
						map.put(key, bundle.getString(key));
				}
			} catch (Throwable t) {
				// Nothing to do
			}
		}
		return map;
	}

	public static List<String> getLocales() {
		List<String> locales = new ArrayList<>();
		Properties p = new Properties();
		try {
			p.load(I18N.class.getResourceAsStream("/i18n/i18n.properties"));
			StringTokenizer st = new StringTokenizer(p.getProperty("locales"), ",", false);
			while (st.hasMoreTokens())
				locales.add(st.nextToken());
		} catch (IOException e) {
			// Nothing to do
		}
		return locales;
	}

	/**
	 * Get the locale corresponding to the ISO 639-2(3 digits)
	 * 
	 * @param iso3Code The 3 digit code
	 * @return The localse
	 */
	public static Locale getLocaleISO3(String iso3Code) {
		return iso3LocaleMap.get(iso3Code.toLowerCase());
	}
}