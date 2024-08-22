package com.logicaldoc.core.metadata;

import com.logicaldoc.core.security.SecurableExtensibleObject;

/**
 * A base class for attribute sets and templates
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.8.3
 */
public class AbstractAttributeSet extends SecurableExtensibleObject {

	private static final long serialVersionUID = 1L;

	/**
	 * The AttributeSet's default type: 0
	 */
	public static final int TYPE_DEFAULT = 0;

	private String name;

	private String label;

	private String description;

	private int readonly = 0;

	private int type = TYPE_DEFAULT;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getReadonly() {
		return readonly;
	}

	public void setReadonly(int readonly) {
		this.readonly = readonly;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		AbstractAttributeSet other = (AbstractAttributeSet) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}