package com.logicaldoc.util.exec;

/**
 * An error happened during command execution
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class ExecException extends Exception {
	private static final long serialVersionUID = 1L;

	public ExecException(String command) {
		this(command, null);
	}

	public ExecException(String command, Throwable cause) {
		super("Failed to launch java program: " + command, cause);
	}
}