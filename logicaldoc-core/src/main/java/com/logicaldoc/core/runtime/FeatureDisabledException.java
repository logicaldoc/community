package com.logicaldoc.core.runtime;

import com.logicaldoc.i18n.I18N;

/**
 * Raised when trying to use a disabled feature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class FeatureDisabledException extends Exception {

    private static final long serialVersionUID = 1L;

    protected FeatureDisabledException(String featureId, String bundlePrefix, String messageTemplate, Throwable cause) {
        super(messageTemplate.formatted(I18N.message("%s%s".formatted(bundlePrefix, featureId))), cause);
    }

    protected FeatureDisabledException(int featureId, Throwable cause) {
        this(Integer.toString(featureId), "feature.Feature_", "Feature '%s' disabled", cause);
    }

    protected FeatureDisabledException(int featureId) {
        this(featureId, null);
    }

    public FeatureDisabledException(Feature feature) {
        this(feature.ordinal() + 1);
    }

    public FeatureDisabledException(Feature feature, Throwable cause) {
        this(feature.ordinal() + 1, cause);
    }
}