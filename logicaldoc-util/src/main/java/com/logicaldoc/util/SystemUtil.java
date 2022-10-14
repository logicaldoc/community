package com.logicaldoc.util;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.management.ManagementFactory;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility methods tpo get informations from the system
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class SystemUtil {

	private static Logger log = LoggerFactory.getLogger(SystemUtil.class);

	public static final String SUN_JAVA_COMMAND = "sun.java.command";

	private static String OS = System.getProperty("os.name").toLowerCase();

	public static void main(String[] args) {
		printEnvironment();
		System.out.println(printStackTrace());
	}

	public static String printEnvironment() {
		StringBuffer sb = new StringBuffer();
		sb.append(printSystemEnvironment());
		sb.append("\n\n");
		sb.append(printJavaEnvironment());
		return sb.toString();
	}

	public static String printStackTrace() {
		StringBuffer sb = new StringBuffer();
		StackTraceElement[] elements = Thread.currentThread().getStackTrace();
		for (StackTraceElement element : elements) {
			if(element.getClassName().equals(Thread.class.getName()) && element.getMethodName().equals("getStackTrace"))
				continue;
			if(element.getClassName().equals(SystemUtil.class.getName()) && element.getMethodName().equals("printStackTrace"))
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

	public static boolean isWindows() {
		return (OS.indexOf("win") >= 0);
	}

	public static boolean isMac() {
		return (OS.indexOf("mac") >= 0);
	}

	public static boolean isUnix() {
		return (OS.indexOf("nix") >= 0 || OS.indexOf("nux") >= 0 || OS.indexOf("aix") >= 0);
	}

	public static boolean isSolaris() {
		return (OS.indexOf("sunos") >= 0);
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

	/**
	 * Restarts the current Java application
	 * 
	 * @param runBeforeRestart some custom code to be run before restarting
	 * 
	 * @throws IOException raised if the application cannot be restarted 
	 */
	public static void restartApplication(Runnable runBeforeRestart) throws IOException {
		try {
			// java binary
			String java = System.getProperty("java.home") + "/bin/java";
			// vm arguments
			List<String> vmArguments = ManagementFactory.getRuntimeMXBean().getInputArguments();
			StringBuffer vmArgsOneLine = new StringBuffer();
			for (String arg : vmArguments) {
				// if it's the agent argument : we ignore it otherwise the
				// address of the old application and the new one will be in
				// conflict
				if (!arg.contains("-agentlib")) {
					vmArgsOneLine.append(arg);
					vmArgsOneLine.append(" ");
				}
			}
			// init the command to execute, add the vm args
			final StringBuffer cmd = new StringBuffer("\"" + java + "\" " + vmArgsOneLine);

			// program main and program arguments
			String[] mainCommand = System.getProperty(SUN_JAVA_COMMAND).split(" ");
			// program main is a jar
			if (mainCommand[0].endsWith(".jar")) {
				// if it's a jar, add -jar mainJar
				cmd.append("-jar " + new File(mainCommand[0]).getPath());
			} else {
				// else it's a .class, add the classpath and mainClass
				cmd.append("-cp \"" + System.getProperty("java.class.path") + "\" " + mainCommand[0]);
			}
			// finally add program arguments
			for (int i = 1; i < mainCommand.length; i++) {
				cmd.append(" ");
				cmd.append(mainCommand[i]);
			}

			// execute the command in a shutdown hook, to be sure that all the
			// resources have been disposed before restarting the application
			Runtime.getRuntime().addShutdownHook(new Thread() {
				@Override
				public void run() {
					try {
						final String message = "Restarting java: " + cmd.toString();
						log.warn(message);
						System.out.println(message);
						Runtime.getRuntime().exec(cmd.toString());
					} catch (IOException e) {
						log.error(e.getMessage(), e);
					}
				}
			});
			// execute some custom code before restarting
			if (runBeforeRestart != null)
				runBeforeRestart.run();

			// exit
			System.exit(0);
		} catch (Exception e) {
			// something went wrong
			throw new IOException("Error while trying to restart the application", e);
		}
	}
}