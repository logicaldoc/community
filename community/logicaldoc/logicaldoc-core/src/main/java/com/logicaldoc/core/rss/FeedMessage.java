package com.logicaldoc.core.rss;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents one RSS message
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.1
 */
public class FeedMessage extends PersistentObject {

	private String title;

	private String description;

	private String link;

	private String author;

	private String guid;

	private Date pubDate;

	private int read = 0;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public String getGuid() {
		return guid;
	}

	public void setGuid(String guid) {
		this.guid = guid;
	}

	public Date getPubDate() {
		return pubDate;
	}

	public void setPubDate(Date pubDate) {
		this.pubDate = pubDate;
	}

	@Override
	public String toString() {
		return pubDate + " - " + guid + ": " + title;
	}

	public int getRead() {
		return read;
	}

	public void setRead(int read) {
		this.read = read;
	}
}