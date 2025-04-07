package com.logicaldoc.core.exception;

import com.logicaldoc.i18n.I18N;

/**
 * Raised when trying to use a disabled feature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class FeatureDisabledException extends Exception {

	private static final long serialVersionUID = 1L;

	public FeatureDisabledException(int featureId) {
		super("Feature " + I18N.message("feature.Feature_" + featureId));
	}

	public FeatureDisabledException(int featureId, Throwable cause) {
		super("Feature " + I18N.message("feature.Feature_" + featureId), cause);
	}
}