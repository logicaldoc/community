package com.logicaldoc.core.metadata;

/**
 * A base class for attribute sets and templates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since <product_release>
 *
 */
public class AbstractAttributeSet extends ExtensibleObject {
	private static final long serialVersionUID = 1L;

	public static int TYPE_DEFAULT = 0;

	private String name;

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
}
