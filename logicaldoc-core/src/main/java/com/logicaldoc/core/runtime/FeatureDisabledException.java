package com.logicaldoc.core.runtime;

import com.logicaldoc.i18n.I18N;

/**
 * Raised when trying to use a disabled feature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class FeatureDisabledException extends Exception {

	private static final String MESSAGE_PREFIX = "Feature ";

	private static final String BUNDLE_PREFIX = "feature.Feature_";
	
	private static final long serialVersionUID = 1L;

	protected FeatureDisabledException(String featureId, String messagePrefix, String featureKeyPrefix,
			Throwable cause) {
		super(messagePrefix + I18N.message(featureKeyPrefix + featureId), cause);
	}

	protected FeatureDisabledException(String featureId, String messagePrefix, String featureKeyPrefix) {
		super(messagePrefix + I18N.message(featureKeyPrefix + featureId));
	}

	public FeatureDisabledException(Feature feature) {
		this(Integer.toString(feature.ordinal() + 1), MESSAGE_PREFIX, BUNDLE_PREFIX);
	}

	public FeatureDisabledException(String featureId) {
		this(featureId, MESSAGE_PREFIX, BUNDLE_PREFIX);
	}

	public FeatureDisabledException(Feature feature, Throwable cause) {
		this(Integer.toString(feature.ordinal() + 1), MESSAGE_PREFIX, BUNDLE_PREFIX, cause);
	}

	public FeatureDisabledException(String featureId, Throwable cause) {
		this(featureId, MESSAGE_PREFIX, BUNDLE_PREFIX, cause);
	}
}