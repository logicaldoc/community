package com.logicaldoc.webservice.doc.view.simple;

public class SimpleClassNameDisplayUtils {

	private SimpleClassNameDisplayUtils() {
	}

	public static String display(String className) {
		StringBuilder sb = new StringBuilder();
		int firstIndex = 0;
		int lastIndex = className.length() - 1;
		for (int i = firstIndex; i <= lastIndex; i++) {
			CharType thisCharType = getCharType(className.charAt(i));
			CharType lastCharType = null;
			CharType nextCharType = null;

			if (i > firstIndex) {
				lastCharType = getCharType(className.charAt(i - 1));
			}

			if (i < lastIndex) {
				nextCharType = getCharType(className.charAt(i + 1));
			}

			if (shouldInsertBlank(thisCharType, lastCharType, nextCharType)) {
				sb.append(" ");
			}

			sb.append(className.charAt(i));
		}
		return sb.toString();
	}

	private static boolean shouldInsertBlank(CharType thisCharType, CharType lastCharType, CharType nextCharType) {

		if (thisCharType == CharType.LOWER_CASE) {
			if (lastCharType == null) { // first letter
				return false;
			}
			if (lastCharType == CharType.LOWER_CASE) {
				return false; // aa
			}

			if (lastCharType == CharType.OTHER) {
				return false; // Aa
			}
		}

		if (thisCharType == CharType.OTHER) {
			if (lastCharType == null) { // first letter
				return false;
			}
			if (lastCharType == CharType.LOWER_CASE) {
				return true; // aA
			}

			if (lastCharType == CharType.OTHER) {
				return nextCharType == CharType.LOWER_CASE;
			}
		}

		throw new IllegalStateException("Unreachable statement");

	}

	private static boolean isLowerCase(char c) {
		return c <= 'z' && c >= 'a';
	}

	private static CharType getCharType(char c) {
		if (isLowerCase(c)) {
			return CharType.LOWER_CASE;
		}

		return CharType.OTHER;
	}

	private static enum CharType {
		LOWER_CASE, OTHER
	}

}
