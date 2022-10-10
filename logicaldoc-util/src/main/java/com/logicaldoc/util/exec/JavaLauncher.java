package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * JavaLauncher Provides an easy way to launch java applications.
 */
public class JavaLauncher {
	protected static Logger log = LoggerFactory.getLogger(JavaLauncher.class);

	private static boolean debug = false;

	/**
	 * Launch a java program.
	 * 
	 * @param mainClass - class with main method
	 * @param classpath - the java classpath
	 * @param jvmargs - arguments for the jvm
	 * @param properties - any system properties
	 * 
	 * @throws Exception An error happened during execution
	 * 
	 * @return the launched process
	 */
	public static Process exec(String mainClass, String classpath, String[] jvmargs, String[] properties)
			throws Exception {

		// get a jvm to execute with
		String jvm = findJVM();

		StringBuffer strClasspath = new StringBuffer("." + File.pathSeparator + classpath);

		// combine all the arguments into 1 array.
		String[] allArguments = new String[properties.length + jvmargs.length];
		System.arraycopy(jvmargs, 0, allArguments, 0, jvmargs.length);
		System.arraycopy(properties, 0, allArguments, jvmargs.length, properties.length);

		// build the command with jvm, arguments, and mainclass
		String[] command = new String[5 + allArguments.length];

		// put java command in place
		command[0] = jvm;

		// add all the arguments
		System.arraycopy(allArguments, 0, command, 1, allArguments.length);

		command[allArguments.length + 2] = "-classpath";
		command[allArguments.length + 3] = "\"" + strClasspath + "\"";
		command[allArguments.length + 4] = mainClass;

		// combine to printable string for debugging
		StringBuffer wholeCommand = new StringBuffer();
		for (int i = 0; i < command.length; i++) {
			wholeCommand.append(command[i] + " ");
		}

		log.info("Executing Command: " + wholeCommand);

		try {

			Process proc = Runtime.getRuntime().exec(command);

			if (debug) {
				monitorProcess(proc);
			}
			return proc;

		} catch (Exception e) {
			log.error("Failed to launch java program: {}", e.getMessage());
			throw new Exception("Failed to launch java program: " + e.getMessage());
		}

	}

	/**
	 * Execute a java jar file that contains a manifest.
	 * 
	 * @param pathToJar - absolute path to your jar
	 * @param jvmargs - arguments for the java virtual machine
	 * @return Process The launched process
	 * 
	 * @throws Exception An error happened during execution
	 */
	public static Process execJar(String pathToJar, String[] jvmargs) throws Exception {
		String jvm = findJVM();

		String[] command = new String[jvmargs.length + 3];
		command[0] = jvm;

		// copy arguments into command
		System.arraycopy(jvmargs, 0, command, 1, jvmargs.length);

		command[jvmargs.length + 1] = "-jar";
		command[jvmargs.length + 2] = new File(pathToJar).getAbsolutePath();

		// combine to printable string for debugging
		StringBuffer wholeCommand = new StringBuffer();
		for (int i = 0; i < command.length; i++) {
			wholeCommand.append(command[i] + " ");
		}

		log.info("Executing Command: " + wholeCommand);

		try {

			Process proc = Runtime.getRuntime().exec(command);

			if (debug) {
				monitorProcess(proc);
			}
			return proc;

		} catch (Exception e) {
			throw new Exception("Failed to launch java program: " + e.getMessage());
		}

	}

	/**
	 * Monitor an execute java program for errors and exit status.
	 * 
	 * @param proc
	 * @throws java.io.IOException
	 */
	private static void monitorProcess(Process proc) throws IOException {
		proc.getInputStream().close();
		proc.getErrorStream().close();

		// Read updaters output
		InputStream inputstream = proc.getErrorStream();
		InputStreamReader inputstreamreader = new InputStreamReader(inputstream);
		BufferedReader bufferedreader = new BufferedReader(inputstreamreader);

		// read the output
		String line;
		while ((line = bufferedreader.readLine()) != null) {
			log.info(line);
		}

		// check for failure
		try {

			if (proc.waitFor() != 0) {
				log.info("exit value = " + proc.exitValue());
			}

		} catch (InterruptedException e) {
			log.error(e.getMessage());
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Find a suitable JVM on the user's system.
	 * 
	 * @return - path to java binary
	 */
	public static String findJVM() {

		String jvm = null;
		jvm = System.getProperty("java.home");

		// handle property not set
		if (jvm == null) {

			log.warn("Java home property not set, just guessing with a general java call, and will probably fail.");

			// just take a guess an hope it's in the classpath
			jvm = "java";

		}

		// add binary folder
		jvm = jvm + File.separator + "bin" + File.separator + "java";

		return jvm;
	}

	/**
	 * Demo - Launch a java program.
	 * 
	 * @param args the invocation arguments
	 */
	public static void main(String[] args) {

		try {

			// things you want the JVM to get. NOT program arguments.
			String[] jvmargs = { "-Xms256m", "-Xmx1024m", "-Ddebug=true" };

			// launch the Jar
			Process proc = JavaLauncher.execJar("Updater.jar", jvmargs);

			// read stderr from new java program
			JavaLauncher.monitorProcess(proc);

		} catch (Exception e) {
			log.error("Failed to launch java program: " + e.getMessage());
		}
	}
}