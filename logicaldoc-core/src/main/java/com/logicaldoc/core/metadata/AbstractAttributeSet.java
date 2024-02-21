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
	public boolean equals(Object obj) {
		if (obj instanceof AbstractAttributeSet other)
			return other.getId() == this.getId();
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}