package com.logicaldoc.core.automation;

import org.apache.commons.lang.StringUtils;

/**
 * Error happening during automation execution
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class AutomationException extends Exception {

	private static final long serialVersionUID = 1L;

	public AutomationException(String errorMessage, String expression) {
		super(String.format("Error '%s' in the script %s", errorMessage, StringUtils.abbreviate(expression, 150)));
	}

	public AutomationException(String expression, Throwable cause) {
		super(String.format("Error '%s' in the script %s", cause.getMessage(), StringUtils.abbreviate(expression, 150)),
				cause);
	}
}