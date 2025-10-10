package com.logicaldoc.core.automation;

/**
 * Exception raised when suspicious code has been detected
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class ForbiddenCodeException extends AutomationException {

	private static final long serialVersionUID = 1L;

	public ForbiddenCodeException(String snippet, String expression) {
		super(String.format("Detected use of the forbidden class java.lang.Runtime: %s", snippet), expression);
	}

	public ForbiddenCodeException(String expression) {
		super("Detected use of the forbidden class java.lang.Runtime", expression);
	}
}