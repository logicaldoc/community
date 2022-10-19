package com.logicaldoc.util.charset;

import javax.mail.internet.MimeUtility;

/**
 * Some utility methods related to charset
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class CharsetUtil {

	private CharsetUtil() {
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
		if (mycc.contains("fileNameCharset=")) {
			mycc = mycc.substring(mycc.indexOf("fileNameCharset=") + "fileNameCharset=".length());
			String mychs = mycc.substring(0, mycc.indexOf(";"));
			String mychs2 = MimeUtility.javaCharset(mychs);
			return mychs2;
		}
		return "UTF-8";
	}
}