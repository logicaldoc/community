package com.logicaldoc.util.exec;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.logicaldoc.util.io.ResourceUtil;

/**
 * This class is responsible for allowing the java program to launch a command
 * with administrator permissions. The way of achieving this greatly varies
 * among the platforms. The JDK classes are of not help here as there is no way
 * to tell a JVM to run as a different user but to launch a new one.
 */
public class PrivilegedRunner {

	private static final String OS_NAME = "os.name";

	private static final Logger logger = Logger.getLogger(PrivilegedRunner.class.getName());

	public int executeWithElevatedRights(String command) throws IOException, InterruptedException {
		ProcessBuilder builder = new ProcessBuilder(getElevator(command));

		if (logger.isLoggable(Level.INFO)) {
			logger.info("Relaunching: " + builder.command());
		}
		Process process = builder.start();
		return process.waitFor();
	}

	protected List<String> getElevator(String command) throws IOException {
		List<String> elevator = new ArrayList<>();

		if (isMac()) {
			elevator.add(extractMacElevator().getCanonicalPath());
			elevator.add(command);
		} else if (isUnix()) {
			elevator.add("xterm");
			elevator.add("-title");
			elevator.add("LogicalDOC");
			elevator.add("-e");
			elevator.add("sudo");
			elevator.add(command);
		} else if (isWindows()) {
			elevator.add("wscript");
			elevator.add(extractVistaElevator().getCanonicalPath());
			elevator.add(command);
		}

		return elevator;
	}

	protected File extractVistaElevator() throws IOException {
		String path = System.getProperty("java.io.tmpdir") + File.separator + "Elevator.js";
		File elevator = new File(path);

		FileOutputStream out = new FileOutputStream(elevator);
		InputStream in = getClass().getResourceAsStream("/com/logicaldoc/util/exec/windows/elevate.js");
		copyStream(out, in);
		in.close();
		out.close();

		elevator.deleteOnExit();
		return elevator;
	}

	protected File extractMacElevator() throws IOException {
		String path = System.getProperty("java.io.tmpdir") + File.separator + "Elevator";
		File elevator = new File(path);

		try (FileOutputStream out = new FileOutputStream(elevator);
				InputStream in = ResourceUtil
						.getInputStream("com/logicaldoc/util/exec/mac/run-with-privileges-on-osx");) {
			copyStream(out, in);
		}

		if (!elevator.setExecutable(true)) {
			throw new IOException("Failed to set execute permission on " + path);
		}

		elevator.deleteOnExit();
		return elevator;
	}

	private void copyStream(OutputStream out, InputStream in) throws IOException {
		byte[] buffer = new byte[1024];
		int bytesRead;
		while ((bytesRead = in.read(buffer)) >= 0) {
			out.write(buffer, 0, bytesRead);
		}
	}

	public static boolean isWindows() {
		return System.getProperty(OS_NAME).toLowerCase().indexOf("win") >= 0;
	}

	public static boolean isMac() {
		return (System.getProperty(OS_NAME).toLowerCase().indexOf("mac") >= 0);
	}

	public static boolean isUnix() {
		return (System.getProperty(OS_NAME).toLowerCase().indexOf("nix") >= 0
				|| System.getProperty(OS_NAME).toLowerCase().indexOf("nux") >= 0
				|| System.getProperty(OS_NAME).toLowerCase().indexOf("aix") >= 0);
	}
}
