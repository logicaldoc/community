package com.logicaldoc.core.exception;

/**
 * Raised when an aspect is disabled
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class AspectDisabledException extends FeatureDisabledException {

	private static final long serialVersionUID = 1L;

	public AspectDisabledException(String aspectId) {
		super(aspectId, "Aspect ", "aspect.");
	}

	public AspectDisabledException(String aspectId, Throwable cause) {
		super(aspectId, "Aspect ", "aspect.", cause);
	}
}