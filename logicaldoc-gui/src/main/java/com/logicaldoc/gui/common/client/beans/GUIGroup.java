package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a users group
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIGroup implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private String name = "";

	private String source;

	private String description = "";

	public static final int TYPE_DEFAULT = 0;

	public static final int TYPE_USER = 1;

	private int type = TYPE_DEFAULT;

	// Optional group from which to import policies
	private Long inheritGroupId;

	public GUIGroup() {
	}

	public GUIGroup(long id) {
		this.id = id;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

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

	public Long getInheritGroupId() {
		return inheritGroupId;
	}

	public void setInheritGroupId(Long inheritGroupId) {
		this.inheritGroupId = inheritGroupId;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}
}
