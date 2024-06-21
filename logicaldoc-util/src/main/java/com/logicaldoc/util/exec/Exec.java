package com.logicaldoc.util.exec;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class used to execute system commands
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class Exec {

	private static final String COMMAND_FAILED_TO_EXECUTE = "Command failed to execute - {}";

	private static final String TIMEOUT_COMMAND = "Timeout command {}";

	private static final String ALLOWED_COMMANDS = "/allowed-commands.txt";

	protected static Logger log = LoggerFactory.getLogger(Exec.class);

	/**
	 * For those methods that collect the output, this is the prefix to use for
	 * the output print. Use null for disabling the out print.
	 */
	private String outPrefix = "out";

	/**
	 * For those methods that collect the output, this is the prefix to use for
	 * the error print. Use null for disabling the error print.
	 */
	private String errPrefix = "err";

	public void setErrPrefix(String errPrefix) {
		this.errPrefix = errPrefix;
	}

	public void setOutPrefix(String outPrefix) {
		this.outPrefix = outPrefix;
	}

	/**
	 * Checks if you are running on Windows
	 * 
	 * @return true if the execution platform is Windows
	 */
	public boolean isWindows() {
		return System.getProperty("os.name").toLowerCase().indexOf("win") >= 0;
	}

	/**
	 * Executes the command by using the process builder.
	 * 
	 * @return the return value
	 * 
	 * @param commandLine The command line to process. The path of the command
	 *        must be listed in the allowed-commands.txt
	 * @param directory The folder where the command will be executed
	 * @param timeout The timeout in seconds
	 * 
	 * @throws IOException raised in case of errors during execution
	 */
	public int exec2(List<String> commandLine, File directory, int timeout) throws IOException {
		checkAllowed(commandLine.get(0));

		log.debug("Executing command: {}", commandLine);
		ProcessBuilder pb = new ProcessBuilder();
		pb.redirectErrorStream(true);
		pb.command(commandLine);
		pb.directory(directory);

		Process process = pb.start();

		StreamGobbler outputGobbler = new StreamGobbler(process.getInputStream());
		outputGobbler.start();

		Worker worker = new Worker(process);
		worker.start();

		try {
			if (timeout > 0)
				worker.join(timeout * 1000L);
			else
				worker.join();
			if (worker.getExit() == null)
				throw new TimeoutException();
			else
				return worker.getExit();
		} catch (TimeoutException e) {
			log.error("Command timed out {}", commandLine);
		} catch (InterruptedException ex) {
			worker.interrupt();
			Thread.currentThread().interrupt();
		} finally {
			try {
				process.destroy();
			} catch (Exception t) {
				// Nothing to do
			}
		}

		return 1;
	}

	/**
	 * Execute the command by using the Runtime.getRuntime().exec()
	 * 
	 * @param commandLine the list of elements in the command line. The path of
	 *        the command must be listed in the allowed-commands.txt
	 *
	 * @return the command execution return value
	 * 
	 * @throws IOException raised in case of errors during execution
	 */
	public int exec(List<String> commandLine) throws IOException {
		return exec(commandLine, null, null, -1);
	}

	/**
	 * Execute the command by using the Runtime.getRuntime().exec()
	 * 
	 * @param commandLine the list of elements in the command line. The path of
	 *        the command must be listed in the allowed-commands.txt
	 * @param env the environment variables
	 * @param dir the current folder
	 * @param timeout maximum execution time expressed in seconds
	 * 
	 * @throws IOException raised in case of errors during execution
	 * 
	 * @return the return code of the command
	 */
	public int exec(final List<String> commandLine, String[] env, File dir, int timeout) throws IOException {
		checkAllowed(commandLine.get(0));

		int exit = 0;

		final Process process = Runtime.getRuntime().exec(commandLine.toArray(new String[0]), env, dir);

		if (timeout > 0) {
			ExecutorService service = Executors.newSingleThreadExecutor();
			try {
				Callable<Integer> call = new CallableProcess(process);
				Future<Integer> future = service.submit(call);
				exit = future.get(timeout, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
				process.destroy();
				Thread.currentThread().interrupt();
			} catch (TimeoutException e) {
				process.destroy();
				log.warn(TIMEOUT_COMMAND, commandLine);
			} catch (Exception e) {
				log.warn(COMMAND_FAILED_TO_EXECUTE, commandLine);
				exit = 1;
			} finally {
				service.shutdown();
			}
		}

		String commandForLog = " (" + commandForLog(commandLine.stream().collect(Collectors.joining(" "))) + ")";

		StreamEater errEater = new StreamEater(errPrefix + commandForLog, process.getErrorStream());

		StreamEater outEater = new StreamEater(outPrefix + commandForLog, process.getInputStream());

		Thread a = new Thread(errEater);
		a.start();

		Thread b = new Thread(outEater);
		b.start();

		try {
			exit = process.waitFor();
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		try {
			process.destroy();
		} catch (Exception t) {
			// Nothing to do
		}

		return exit;
	}

	/**
	 * Takes the first part and the last part of a command line to make it
	 * usable in the logs
	 * 
	 * @param commandLine the command line to process
	 * 
	 * @return The elaboration suitable for log
	 */
	protected String commandForLog(final String commandLine) {
		if (commandLine.length() <= 60)
			return commandLine;
		else
			return StringUtils.left(commandLine, 30) + "..." + StringUtils.right(commandLine, 30);
	}

	/**
	 * Execute the command by using the Runtime.getRuntime().exec()
	 * 
	 * @param commandLine the command line string. The path of the command must
	 *        be listed in the allowed-commands.txt
	 * @param env the execution environment
	 * @param dir the current folder
	 * 
	 * @return The output of the command
	 * 
	 * @throws IOException If the execution caused an error
	 */
	public String exec(String commandLine, String[] env, File dir) throws IOException {
		checkAllowed(commandLine);

		Process process = Runtime.getRuntime().exec(commandLine, env, dir != null ? dir : null);
		BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
		StringBuilder out = new StringBuilder();
		String s;
		while ((s = reader.readLine()) != null)
			out.append(s);
		return out.toString();
	}

	public int exec(final String commandLine, String[] env, File dir, StringBuilder buffer, int timeout)
			throws IOException {
		checkAllowed(commandLine);

		int exit = 0;

		final Process process = Runtime.getRuntime().exec(commandLine, env, dir);

		String commandForLog = " (" + commandForLog(commandLine) + ")";
		StreamEater errEater = new StreamEater(errPrefix + commandForLog, process.getErrorStream());

		StreamEater outEater = new StreamEater(outPrefix + commandForLog, process.getInputStream(), buffer);

		Thread a = new Thread(errEater);
		a.start();

		Thread b = new Thread(outEater);
		b.start();
		
		if (timeout > 0) {
			ExecutorService service = Executors.newSingleThreadExecutor();
			try {
				Callable<Integer> call = new CallableProcess(process);
				Future<Integer> future = service.submit(call);
				exit = future.get(timeout, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
				process.destroy();
				Thread.currentThread().interrupt();
			} catch (TimeoutException e) {
				process.destroy();
				log.warn(TIMEOUT_COMMAND, commandLine);
			} catch (Exception e) {
				log.warn(COMMAND_FAILED_TO_EXECUTE, commandLine);
				exit = 1;
			} finally {
				service.shutdown();
			}
		}

		try {
			exit = process.waitFor();
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		try {
			process.destroy();
		} catch (Exception t) {
			// Nothing to do
		}

		return exit;
	}

	public int exec(final String commandLine, String[] env, File dir, Writer outputWriter, int timeout)
			throws IOException {
		checkAllowed(commandLine);

		int exit = 0;

		final Process process = Runtime.getRuntime().exec(commandLine, env, dir);

		if (timeout > 0) {
			ExecutorService service = Executors.newSingleThreadExecutor();
			try {
				Callable<Integer> call = new CallableProcess(process);
				Future<Integer> future = service.submit(call);
				exit = future.get(timeout, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
				process.destroy();
				Thread.currentThread().interrupt();
			} catch (TimeoutException e) {
				process.destroy();
				log.warn(TIMEOUT_COMMAND, commandLine);
			} catch (Exception e) {
				log.warn(COMMAND_FAILED_TO_EXECUTE, commandLine);
				exit = 1;
			} finally {
				service.shutdown();
			}
		}

		String commandForLog = " (" + commandForLog(commandLine) + ")";

		StreamEater errEater = new StreamEater(errPrefix + commandForLog, process.getErrorStream());

		StreamEater outEater = new StreamEater(outPrefix + commandForLog, process.getInputStream(), outputWriter);

		Thread a = new Thread(errEater);
		a.start();

		Thread b = new Thread(outEater);
		b.start();

		try {
			exit = process.waitFor();
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		try {
			process.destroy();
		} catch (Exception t) {
			// Nothing to do
		}

		if (outputWriter != null) {
			outputWriter.flush();
		}

		return exit;
	}

	/**
	 * Execute the command by using the Runtime.exec
	 * 
	 * @param commandLine the command line. The path of the command must be
	 *        listed in the allowed-commands.txt
	 * @param env the environment variables
	 * @param dir the current execution directory
	 * @param timeout maximum execution time expressed in seconds
	 * 
	 * @return the command return value
	 * 
	 * @throws IOException raised if the command produced an error
	 */
	public int exec(final String commandLine, String[] env, File dir, int timeout) throws IOException {
		return exec(commandLine, env, dir, (Writer) null, timeout);
	}

	static class CallableProcess implements Callable<Integer> {
		private Process p;

		public CallableProcess(Process process) {
			p = process;
		}

		public Integer call() throws Exception {
			return p.waitFor();
		}
	}

	/**
	 * Checks if a given command is allowed
	 * 
	 * @param commandLine the command line to test
	 * 
	 * @throws IOException if the command is not allowed
	 */
	private static void checkAllowed(String commandLine) throws IOException {
		Set<String> allowedCommands = getAllowedCommands();
		if (allowedCommands.isEmpty())
			return;

		String cmdLine = commandLine;
		if (cmdLine.startsWith("\""))
			cmdLine = commandLine.substring(1);

		boolean allowed = allowedCommands.contains(cmdLine);
		if (!allowed) {
			for (String row : allowedCommands) {
				if ("*".equals(row) || cmdLine.startsWith(row)) {
					allowed = true;
					break;
				}
			}
		}

		if (!allowed)
			throw new IOException("Command " + commandLine + " is not allowed. Add it to allowed-commands.txt.");
	}

	/**
	 * Retrieves the full list of the OS commands allowed to execute
	 * 
	 * @return the set of allowed commands
	 */
	private static Set<String> getAllowedCommands() {
		Set<String> allowedCommands = new HashSet<>();
		URL resource = Exec.class.getResource(ALLOWED_COMMANDS);
		if (resource == null)
			return allowedCommands;

		InputStream is = null;
		try {
			try {
				is = new FileInputStream(resource.getFile());
			} catch (FileNotFoundException e) {
				log.error(e.getMessage(), e);
			}

			if (is == null)
				is = Exec.class.getResourceAsStream(ALLOWED_COMMANDS);

			try (Scanner sc = new Scanner(is);) {
				while (sc.hasNext()) {
					String line = sc.nextLine().trim();
					if (StringUtils.isNotEmpty(line) && !line.startsWith("#") && !allowedCommands.contains(line))
						allowedCommands.add(line);
				}
			}
		} finally {
			close(is);
		}

		return allowedCommands;
	}

	private static void close(InputStream is) {
		if (is != null)
			try {
				is.close();
			} catch (IOException e) {
				// Nothing to do
			}
	}

	/**
	 * Utility method for normalizing a path to be used to invoke a command
	 * 
	 * @param srcPath the source path to parse
	 * 
	 * @return the normalized path
	 */
	public String normalizePathForCommand(String srcPath) {
		String normalizedPath = FilenameUtils.normalize(srcPath);
		if (System.getProperty("os.name").toLowerCase().indexOf("win") >= 0)
			normalizedPath = "\"" + normalizedPath + "\"";
		return normalizedPath;
	}
}