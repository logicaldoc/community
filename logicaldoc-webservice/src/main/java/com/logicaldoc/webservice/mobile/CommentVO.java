package com.logicaldoc.webservice.mobile;

import java.io.Serializable;

import jakarta.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "Item")
public class CommentVO implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private String name;
	private String author;
	private String content;
	private String modifiedOn;
	private String title;
	
	public String getTitle() {
		return title;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public String getModifiedOn() {
		return modifiedOn;
	}

	public void setModifiedOn(String modifiedOn) {
		this.modifiedOn = modifiedOn;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public void setTitle(String title) {
		this.title = title;
	}
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}