package com.logicaldoc.util.charset;

import javax.mail.internet.MimeUtility;

/**
 * Some utility methods related to charset
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class CharsetUtil {

	private static final String FILE_NAME_CHARSET = "fileNameCharset=";

	private CharsetUtil() {
		throw new IllegalStateException("Utility class");
	}

	public static String getCharset(String contentType) {
		String mycc = contentType.replace("\"", "");

		if (mycc.indexOf(" format=flowed") != -1) {
			mycc = mycc.replace(" format=flowed", "");
		}
		if (mycc.indexOf("format=flowed") != -1) {
			mycc = mycc.replace("format=flowed", "");
		}

		if (!mycc.endsWith(";"))
			mycc += ";";
		if (mycc.contains(FILE_NAME_CHARSET)) {
			mycc = mycc.substring(mycc.indexOf(FILE_NAME_CHARSET) + FILE_NAME_CHARSET.length());
			String mychs = mycc.substring(0, mycc.indexOf(";"));
			return MimeUtility.javaCharset(mychs);
		}
		return "UTF-8";
	}
}