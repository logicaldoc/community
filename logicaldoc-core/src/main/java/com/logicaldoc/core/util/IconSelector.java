package com.logicaldoc.core.util;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

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
	private static Set<String> availableIconFiles = new HashSet<>();

	public static Set<String> getAvailableIconFiles() {
		return availableIconFiles;
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

		if (StringUtils.isEmpty(ext) || (!availableIconFiles.isEmpty() && availableIconFiles.contains(icon)))
			icon = "blank";

		return icon;
	}

	private static String normalizeExtension(String ext) {
		if (ext != null)
			ext = ext.contains(".") ? FileUtil.getExtension(ext).toLowerCase() : ext.toLowerCase();
		return ext;
	}
}