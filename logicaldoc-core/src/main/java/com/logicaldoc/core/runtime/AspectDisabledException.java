package com.logicaldoc.core.runtime;

/**
 * Raised when an aspect is disabled
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class AspectDisabledException extends FeatureDisabledException {

	private static final long serialVersionUID = 1L;

	public AspectDisabledException(Aspect aspect) {
		this(aspect, null);
	}

	public AspectDisabledException(Aspect aspect, Throwable cause) {
	    super(aspect.key(), "aspect.", "Aspect '%s' disabled", cause);
	}
}