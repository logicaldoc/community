package com.logicaldoc.gui.common.client.i18n;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.smartgwt.client.types.DateDisplayFormat;

/**
 * Retrieves i18n resources
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class I18N {
	private static final String FORMAT_DATELONG = "format_datelong";

	private static final String FORMAT_DATE = "format_date";

	private static final String FORMAT_DATESHORT = "format_dateshort";

	private static String locale = "en";

	private static List<GUIValue> languages = new ArrayList<>();

	private static List<GUIValue> guiLanguages = new ArrayList<>();

	private static HashMap<String, String> bundle = new HashMap<>();

	private static DateTimeFormat dateFormatLong = null;

	private static DateTimeFormat dateFormatShort = null;

	private static DateTimeFormat dateFormat = null;

	public static String messageWithDefault(String key, String def) {
		if (bundle.containsKey(key))
			return bundle.get(key);
		else if (key != null && bundle.containsKey(key.toLowerCase()))
			return bundle.get(key.toLowerCase());
		else
			return def;
	}

	public static String message(String key) {
		return messageWithDefault(key, key != null ? key : "");
	}

	public static String message(String key, String val) {
		return message(key, val, "");
	}

	public static String message(String key, String val1, String val2) {
		return message(key, val1, val2, "");
	}

	public static String message(String key, String val1, String val2, String val3) {
		return message(key, Arrays.asList(val1, val2, val3));
	}

	public static String message(String key, List<String> vals) {
		String tmp = message(key);
		try {
			for (int i = 0; i < vals.size(); i++) {
				tmp = tmp.replace("{" + i + "}", vals.get(i));
			}
		} catch (Exception t) {
			// Nothing to do
		}
		return tmp;

	}

	public static String getLocale() {
		return locale;
	}

	/**
	 * Computes the default suitable language for documents
	 * 
	 * @return the default locale
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

		return languages.get(0).getCode();
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

	public static Map<String, String> getSupportedLanguages(boolean addEmpty) {
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		if (addEmpty)
			map.put("", " ");
		if (languages != null)
			for (GUIValue l : languages)
				map.put(l.getCode(), l.getValue());
		return map;
	}

	public static Map<String, String> getSupportedGuiLanguages(boolean addEmpty) {
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		if (addEmpty)
			map.put("", " ");
		if (guiLanguages != null)
			for (GUIValue l : guiLanguages)
				map.put(l.getCode(), l.getValue());
		return map;
	}

	public List<GUIValue> getLanguages() {
		return languages;
	}

	public static void setLanguages(List<GUIValue> languages) {
		I18N.languages = languages;
	}

	private static void initBundle(List<GUIValue> messages) {
		bundle.clear();
		for (GUIValue val : messages)
			bundle.put(val.getCode(), val.getValue());

		String[] dayNames = new String[7];
		for (int i = 0; i < 7; i++)
			dayNames[i] = message("dayname_" + i);

		com.smartgwt.client.util.DateUtil.setDayNames(dayNames);
	}

	public static void init(GUIInfo info) {
		setLanguages(info.getSupportedLanguages());
		setGuiLanguages(info.getSupportedGUILanguages());
		initBundle(info.getBundle());

		/*
		 * Prepare the date formatters
		 */
		dateFormat = DateTimeFormat.getFormat(message(FORMAT_DATE));
		dateFormatShort = DateTimeFormat.getFormat(message(FORMAT_DATESHORT));
		dateFormatLong = DateTimeFormat.getFormat(message(FORMAT_DATELONG));
	}

	public static void init(GUIUser user) {
		setLocale(user.getLanguage());

		/*
		 * Prepare the date formatters
		 */
		if (user.getDateFormat() != null && !user.getDateFormat().isEmpty())
			dateFormat = DateTimeFormat.getFormat(user.getDateFormat());
		else
			dateFormat = DateTimeFormat.getFormat(message(FORMAT_DATE));

		if (user.getDateFormatShort() != null && !user.getDateFormatShort().isEmpty())
			dateFormatShort = DateTimeFormat.getFormat(user.getDateFormatShort());
		else
			dateFormatShort = DateTimeFormat.getFormat(message(FORMAT_DATESHORT));

		if (user.getDateFormatLong() != null && !user.getDateFormatLong().isEmpty())
			dateFormatLong = DateTimeFormat.getFormat(user.getDateFormatLong());
		else
			dateFormatLong = DateTimeFormat.getFormat(message(FORMAT_DATELONG));
	}

	public static List<GUIValue> getGuiLanguages() {
		return guiLanguages;
	}

	public static void setGuiLanguages(List<GUIValue> guiLanguages) {
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

	public static String formatDateLong(Date date) {
		if (date == null)
			return null;
		return dateFormatLong.format(new Date(date.getTime()));
	}

	public static DateTimeFormat getDateFormatShort() {
		return dateFormatShort;
	}

	public static DateTimeFormat getDateFormat() {
		return dateFormat;
	}

	public static DateDisplayFormat getDateDisplayFormat(boolean withTime) {
		if ("yyyy/MM/dd".equals(I18N.message(FORMAT_DATESHORT))) {
			if (withTime)
				return DateDisplayFormat.TOJAPANSHORTDATETIME;
			else
				return DateDisplayFormat.TOJAPANSHORTDATE;
		} else if (I18N.message(FORMAT_DATESHORT).contains("MM/dd")) {
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