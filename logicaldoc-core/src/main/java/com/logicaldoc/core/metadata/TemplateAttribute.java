package com.logicaldoc.core.metadata;

import javax.persistence.Column;
import javax.persistence.Embeddable;

@Embeddable
public class TemplateAttribute extends Attribute {

	private static final long serialVersionUID = 1L;

	/**
	 * Optional validation script
	 */
	@Column(name = "ld_validation")
	private String validation;

	/**
	 * Optional script that defines the initial value
	 */
	@Column(name = "ld_initialization")
	private String initialization;

	public TemplateAttribute() {
		super();
	}

	public TemplateAttribute(TemplateAttribute source) {
		super(source);
		this.validation = source.validation;
		this.initialization = source.initialization;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}

	public String getInitialization() {
		return initialization;
	}

	public void setInitialization(String initialization) {
		this.initialization = initialization;
	}

}