package com.logicaldoc.gui.common.client.i18n;

import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.smartgwt.client.types.DateDisplayFormat;

/**
 * Retrieves i18n resources
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class I18N {
	private static String locale = "en";

	private static GUIValue[] languages;

	private static GUIValue[] guiLanguages;

	private static HashMap<String, String> bundle = new HashMap<String, String>();

	private static DateTimeFormat dateFormatShort = null;

	private static DateTimeFormat dateFormat = null;

	public static String messageWithDefault(String key, String def) {
		if (bundle.containsKey(key))
			return bundle.get(key);
		else
			return def;
	}

	public static String message(String key) {
		if (bundle.containsKey(key))
			return bundle.get(key);
		else
			return key;
	}

	public static String message(String key, String val) {
		return message(key, val, null);
	}

	public static String message(String key, String val1, String val2) {
		String tmp = message(key);
		try {
			tmp = tmp.replaceAll("\\{0\\}", val1);
		} catch (Throwable t) {
		}
		try {
			tmp = tmp.replaceAll("\\{1\\}", val2);
		} catch (Throwable t) {
		}
		return tmp;
	}

	public static String message(String key, String[] vals) {
		String tmp = message(key);
		try {
			for (int i = 0; i < vals.length; i++) {
				tmp = tmp.replaceAll("\\{" + i + "\\}", vals[i]);
			}
		} catch (Throwable t) {
		}
		return tmp;

	}

	public static String getLocale() {
		return locale;
	}

	/**
	 * Computes the default suitable language for documents
	 */
	public static String getDefaultLocaleForDoc() {
		// Search for exact match
		for (GUIValue l : languages) {
			if (l.getCode().equals(locale))
				return l.getCode();
		}

		// Check the first 2 letters(the language)
		for (GUIValue l : languages) {
			if (l.getCode().startsWith(locale.substring(0, 2)))
				return l.getCode();
		}

		return languages[0].getCode();
	}

	public static char groupingSepator() {
		String gs = message("grouping_separator");
		return gs.charAt(gs.length() - 1);
	}

	public static char decimalSepator() {
		String gs = message("decimal_separator");
		return gs.charAt(gs.length() - 1);
	}

	public static void setLocale(String locale) {
		I18N.locale = locale;
	}

	public static LinkedHashMap<String, String> getSupportedLanguages(boolean addEmpty) {
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		if (addEmpty)
			map.put("", " ");
		if (languages != null)
			for (GUIValue l : languages) {
				map.put(l.getCode(), l.getValue());
			}
		return map;
	}

	public static LinkedHashMap<String, String> getSupportedGuiLanguages(boolean addEmpty) {
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		if (addEmpty)
			map.put("", " ");
		if (guiLanguages != null)
			for (GUIValue l : guiLanguages) {
				map.put(l.getCode(), l.getValue());
			}
		return map;
	}

	public GUIValue[] getLanguages() {
		return languages;
	}

	public static void setLanguages(GUIValue[] languages) {
		I18N.languages = languages;
	}

	public static void initBundle(GUIValue[] messages) {
		bundle.clear();
		for (GUIValue val : messages) {
			bundle.put(val.getCode(), val.getValue());
		}
	}

	public static void init(GUIInfo info) {
		setLanguages(info.getSupportedLanguages());
		setGuiLanguages(info.getSupportedGUILanguages());
		initBundle(info.getBundle());

		/*
		 * Prepare the date formatters
		 */
		dateFormatShort = DateTimeFormat.getFormat(message("format_dateshort"));
		dateFormat = DateTimeFormat.getFormat(message("format_date"));
	}

	public static void init(GUISession session) {
		init(session.getInfo());
		I18N.locale = session.getUser().getLanguage();
	}

	public static GUIValue[] getGuiLanguages() {
		return guiLanguages;
	}

	public static void setGuiLanguages(GUIValue[] guiLanguages) {
		I18N.guiLanguages = guiLanguages;
	}

	public static String formatDateShort(Date date) {
		if (date == null)
			return null;
		return dateFormatShort.format(new Date(date.getTime()));
	}

	public static String formatDate(Date date) {
		if (date == null)
			return null;
		return dateFormat.format(new Date(date.getTime()));
	}

	public static DateTimeFormat getDateFormatShort() {
		return dateFormatShort;
	}

	public static DateTimeFormat getDateFormat() {
		return dateFormat;
	}

	public static DateDisplayFormat getDateDisplayFormat(boolean withTime) {
		if ("yyyy/MM/dd".equals(I18N.message("format_dateshort"))) {
			if (withTime)
				return DateDisplayFormat.TOJAPANSHORTDATETIME;
			else
				return DateDisplayFormat.TOJAPANSHORTDATE;
		} else if (I18N.message("format_dateshort").contains("MM/dd")) {
			if (withTime)
				return DateDisplayFormat.TOUSSHORTDATETIME;
			else
				return DateDisplayFormat.TOUSSHORTDATE;
		} else {
			if (withTime)
				return DateDisplayFormat.TOEUROPEANSHORTDATETIME;
			else
				return DateDisplayFormat.TOEUROPEANSHORTDATE;
		}
	}

	public static String getAttributeLabel(String name) {
		if (name == null)
			return null;
		String label = name.trim();
		if (label.startsWith("ext_"))
			label = label.substring(4);
		else if ("published".equals(name) || "created".equals(name))
			label = message(name + "on");
		else
			label = message(label.toLowerCase());
		return label;
	}
}