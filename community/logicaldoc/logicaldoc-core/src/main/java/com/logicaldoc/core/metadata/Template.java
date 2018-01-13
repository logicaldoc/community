package com.logicaldoc.core.metadata;

/**
 * A template collects a set of attributesets ant is itself an extensible
 * object.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 4.0
 */
public class Template extends ExtensibleObject {

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