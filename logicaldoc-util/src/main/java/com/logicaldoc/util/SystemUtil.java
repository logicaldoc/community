package com.logicaldoc.util;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.Map;

/**
 * Utility methods to get informations from the system
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class SystemUtil {

	public static String printEnvironment() {
		StringBuilder sb = new StringBuilder();
		sb.append(printSystemEnvironment());
		sb.append("\n\n");
		sb.append(printJavaEnvironment());
		return sb.toString();
	}

	public static String printStackTrace() {
		StringBuilder sb = new StringBuilder();
		StackTraceElement[] elements = Thread.currentThread().getStackTrace();
		for (StackTraceElement element : elements) {
			if ((element.getClassName().equals(Thread.class.getName())
					&& element.getMethodName().equals("getStackTrace"))
					|| (element.getClassName().equals(SystemUtil.class.getName())
							&& element.getMethodName().equals("printStackTrace")))
				continue;
			sb.append(element.getClassName());
			sb.append(".");
			sb.append(element.getMethodName());
			sb.append("(");
			sb.append(element.getLineNumber());
			sb.append(")\n");
		}
		return sb.toString();
	}

	public static String printSystemEnvironment() {
		StringBuilder sb = new StringBuilder();
		Map<String, String> env = System.getenv();

		for (Map.Entry<String, String> entry : env.entrySet()) {
			if (sb.length() > 0)
				sb.append("\n");
			sb.append(entry.getKey());
			sb.append("=");
			sb.append(entry.getValue());
		}
		return "#System Environment\n#" + new Date() + "\n" + sb.toString();
	}

	public static String printJavaEnvironment() {
		StringWriter writer = new StringWriter();
		try {
			System.getProperties().store(new PrintWriter(writer), "Java Environment");
			return writer.getBuffer().toString();
		} catch (IOException e) {
			// Nothing to do
		}
		return "";
	}

	public static boolean isWindows() {
		return (osName().contains("win"));
	}

	private static String osName() {
		return System.getProperty("os.name").toLowerCase();
	}

	public static boolean isMac() {
		return (osName().contains("mac"));
	}

	public static boolean isUnix() {
		return (osName().contains("nix") || osName().contains("nux") || osName().contains("aix"));
	}

	public static boolean isSolaris() {
		return (osName().contains("sunos"));
	}

	public static String getOS() {
		if (isWindows()) {
			return "win";
		} else if (isMac()) {
			return "osx";
		} else if (isUnix()) {
			return "uni";
		} else if (isSolaris()) {
			return "sol";
		} else {
			return "err";
		}
	}
}