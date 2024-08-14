package com.logicaldoc.core.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.logicaldoc.util.io.FileUtil;

/**
 * Utility class to select an icon based on a file extension
 * 
 * @author Sebastian Stein
 */
public abstract class IconSelector {

	/**
	 * An optional set of file icons that are supposed to be available in the
	 * system
	 */
	private static Set<String> availableIcons = new HashSet<>();

	/**
	 * A map of aliases, key is the alias, value is the rel icon to use
	 */
	private static Map<String, String> aliases = new HashMap<>();

	static {
		aliases.put("vsdx", "vsd");

		aliases.put("mkd", "md");
		aliases.put("mdwn", "md");
		aliases.put("mdown", "md");
		aliases.put("mdtxt", "md");
		aliases.put("mdtext", "md");
		aliases.put("markdown", "md");
	}

	private IconSelector() {
		throw new IllegalStateException("Utility class");
	}

	public static Set<String> getAvailableIcons() {
		return availableIcons;
	}

	/**
	 * Returns the icon by parsing the provided file extension
	 * 
	 * @param ext file extension
	 * 
	 * @return file name of the icon
	 */
	public static String selectIcon(String ext) {
		ext = normalizeExtension(ext);

		String icon = ext;
		if (aliases.containsKey(ext))
			icon = aliases.get(ext);

		if (!availableIcons.contains(icon))
			icon = "blank";

		return icon;
	}

	private static String normalizeExtension(String ext) {
		if (ext != null)
			ext = ext.contains(".") ? FileUtil.getExtension(ext).toLowerCase() : ext.toLowerCase();
		return ext;
	}
}