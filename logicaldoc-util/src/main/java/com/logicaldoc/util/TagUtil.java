package com.logicaldoc.util;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.util.config.ContextProperties;

/**
 * Utility class for tags handling
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class TagUtil {

	public static final int MAX_FIELD_LENGTH = 4000;

	private TagUtil() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Detects words in the passed string and creates a list of tags. <br>
	 * <b>Atention:</b> Recognized tags are words of min. MIN_CHARS chars and
	 * max MAX_CHARS characters.
	 * 
	 * @param tenantName name of the tenant
	 * @param words a string of words (tags are separated by ,);
	 * 
	 * @return the collection of tags
	 */
	public static Set<String> extractTags(String tenantName, String words) {
		Set<String> coll = new HashSet<>();
		if (words == null)
			return coll;

		if (!words.contains(","))
			words = "," + words + ",";

		// Replace the escapes \, in _comma_ in order to include those
		// tags with
		// commas inside so the tokenization will work property
		words = words.replace("\\,", "__comma__");

		int minSize = 3;
		int maxSize = 9;

		try {
			ContextProperties conf = new ContextProperties();
			minSize = conf.getInt(tenantName + ".tag.minsize");
			maxSize = conf.getInt(tenantName + ".tag.maxsize");
		} catch (IOException e) {
			// Nothing to do
		}

		StringTokenizer st = new StringTokenizer(words, ",", false);
		while (st.hasMoreTokens()) {
			String word = st.nextToken();

			// Get back comma escapes into the , char
			word = word.replace("__comma__", ",");
			if (StringUtils.isNotEmpty(word)) {
				word = word.trim();
				if (word.length() >= minSize) {
					if (word.length() > maxSize)
						coll.add(word.substring(0, maxSize));
					else
						coll.add(word);
				}
			}
		}

		return coll;
	}

	/**
	 * Normalizes the passed string extracting all tags and producing anither
	 * string containing all tags separated by a comma.
	 * <p>
	 * <b>Atention:</b> The returned string is not more long than
	 * MAX_FIELD_LENGTH characters.
	 * 
	 * @param tenantName Name of the tenant
	 * @param words The string to be normalized
	 * 
	 * @return a string in the form "tag1,tag2,tag3"
	 */
	public static String normalizeTags(String tenantName, String words) {
		// Extract tags ad compose the normalized string
		Set<String> tags = extractTags(tenantName, words);
		StringBuilder sb = new StringBuilder();
		for (String tag : tags) {
			// escape the , with \,
			tag = tag.replace(",", "\\,");
			if (sb.length() > 0)
				sb.append(",");
			sb.append(tag);
		}
		String str = sb.toString();

		// Trim the string if too long
		if (str.length() > MAX_FIELD_LENGTH)
			str = str.substring(0, MAX_FIELD_LENGTH);
		return str;
	}
}