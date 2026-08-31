package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * A template use to compose the outgoing messages
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class GUIMessageTemplate implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;

	private String name;

	private String language;

	private String subject;

	private String body;
	
	private String type;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}
}