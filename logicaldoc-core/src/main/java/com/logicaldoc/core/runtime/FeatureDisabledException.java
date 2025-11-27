package com.logicaldoc.core.runtime;

import javax.xml.catalog.CatalogFeatures.Feature;

import com.logicaldoc.i18n.I18N;

/**
 * Raised when trying to use a disabled feature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class FeatureDisabledException extends Exception {

	private static final long serialVersionUID = 1L;

	protected FeatureDisabledException(String featureId, String messagePrefix, String featureKeyPrefix,
			Throwable cause) {
		super(messagePrefix + I18N.message(featureKeyPrefix + featureId), cause);
	}

	protected FeatureDisabledException(String featureId, String messagePrefix, String featureKeyPrefix) {
		super(messagePrefix + I18N.message(featureKeyPrefix + featureId));
	}

	public FeatureDisabledException(Feature feature) {
		this(Integer.toString(feature.ordinal() + 1), "Feature ", "feature.Feature_");
	}

	public FeatureDisabledException(String featureId) {
		this(featureId, "Feature ", "feature.Feature_");
	}

	public FeatureDisabledException(Feature feature, Throwable cause) {
		this(Integer.toString(feature.ordinal() + 1), "Feature ", "feature.Feature_", cause);
	}
	
	public FeatureDisabledException(String featureId, Throwable cause) {
		this(featureId, "Feature ", "feature.Feature_", cause);
	}
}