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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((validation == null) ? 0 : validation.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Template other = (Template) obj;
		if (validation == null) {
			if (other.validation != null)
				return false;
		} else if (!validation.equals(other.validation))
			return false;
		return true;
	}
}