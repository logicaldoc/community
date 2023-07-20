package com.logicaldoc.util;

import java.util.Arrays;

/**
 * Useful for stripping chars not valid for Linux or Windows
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class FileNameCleaner {

	static final int[] illegalChars = { 34, 60, 62, 124, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
			18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 58, 42, 63, 92, 47 };
	static {
		Arrays.sort(illegalChars);
	}

	private FileNameCleaner() {
	}

	public static String cleanFileName(String badFileName) {
		StringBuilder cleanName = new StringBuilder();
		for (int i = 0; i < badFileName.length(); i++) {
			int c = (int) badFileName.charAt(i);
			if (Arrays.binarySearch(illegalChars, c) < 0) {
				cleanName.append((char) c);
			} else
				cleanName.append('_');
		}
		return cleanName.toString();
	}

	public static String cleanPath(String badPath) {
		StringBuilder cleanName = new StringBuilder();
		for (int i = 0; i < badPath.length(); i++) {
			int c = (int) badPath.charAt(i);
			if (c == '/' || c == '\\' || c == ':' || Arrays.binarySearch(illegalChars, c) < 0) {
				cleanName.append((char) c);
			} else
				cleanName.append('_');
		}
		return cleanName.toString();
	}
}
