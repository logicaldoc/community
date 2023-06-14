package com.logicaldoc.util;

import java.util.Locale;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;

/**
 * Utility methods for Locale handling
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class LocaleUtil {

	private LocaleUtil() {
	}

	/**
	 * Creates the locale from a string
	 * 
	 * @param str String in format <b>language</b>_<b>country</b>_<b>variant</b>
	 * 
	 * @return the locale that corresponds to <code>str</code>
	 */
	public static Locale toLocale(String str) {
		if (StringUtils.isEmpty(str))
			return Locale.ENGLISH;

		str = str.replace('-', '_');

		String lang = "";
		String country = "";
		String variant = "";
		StringTokenizer st = new StringTokenizer(str, "_", false);
		lang = st.nextToken();
		if (st.hasMoreTokens())
			country = st.nextToken();
		if (st.hasMoreTokens())
			variant = st.nextToken();
		return new Locale(lang, country, variant);
	}
}