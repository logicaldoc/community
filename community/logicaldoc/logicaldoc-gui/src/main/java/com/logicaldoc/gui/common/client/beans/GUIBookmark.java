package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * A user's bookmark
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class GUIBookmark implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private String name;

	private String description;

	private Long folderId;

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

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}
}