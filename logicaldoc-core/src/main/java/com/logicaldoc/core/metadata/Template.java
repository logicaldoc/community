package com.logicaldoc.core.metadata;

/**
 * A template collects a set of attributesets ant is itself an extensible
 * object.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class Template extends AbstractAttributeSet {

	private static final long serialVersionUID = 1L;

	private String validation;

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}
}