package com.logicaldoc.util.exception;

/**
 * An exception useful in cycles instead of using the break keywords
 * 
 * @author Marco Meschieri - LogicalDOCs
 * @since 8.8.4
 */
public class StopException extends Exception {

	private static final long serialVersionUID = 1L;

	public StopException() {
		super("stop");
	}
	
	public StopException(String message) {
		super(message);
	}
}