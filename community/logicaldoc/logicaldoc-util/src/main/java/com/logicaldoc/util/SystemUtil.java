package com.logicaldoc.util;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.Map;

/**
 * Utility methods tpo get informations from the system
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class SystemUtil {

	public static void main(String[] args) {
		printEnvironment();
	}

	public static String printEnvironment() {
		StringBuffer sb = new StringBuffer();
		sb.append(printSystemEnvironment());
		sb.append("\n\n");
		sb.append(printJavaEnvironment());
		return sb.toString();
	}
	
	public static String printSystemEnvironment() {
		StringBuffer sb = new StringBuffer();
		Map<String, String> env = System.getenv();
		for (String key : env.keySet()) {
			if (sb.length() > 0)
				sb.append("\n");
			sb.append(key);
			sb.append("=");
			sb.append(env.get(key));
		}
		return "#System Environment\n#" + new Date() + "\n" + sb.toString();
	}

	public static String printJavaEnvironment() {
		StringWriter writer = new StringWriter();
		try {
			System.getProperties().store(new PrintWriter(writer), "Java Environment");
			return writer.getBuffer().toString();
		} catch (IOException e) {

		}
		return "";
	}

}