package com.logicaldoc.webservice.model;

import jakarta.xml.bind.annotation.XmlType;

import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Bookmark.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
@XmlType(name = "WSBookmark")
public class WSBookmark {

	@WSDoc(description = "unique identifier")
	private long id;

	private long userId;

	@WSDoc(description = "identifier of the referenced object")
	private long targetId;

	private String title = "";

	private String description = "";

	private int position = 0;

	// The document file extension
	private String fileType;

	@WSDoc(description = "<b>0</b> = document,  <b>1</b> = folder")
	private int type = Bookmark.TYPE_DOCUMENT;

	public Bookmark toBookmark() {
		Bookmark bookmark = new Bookmark();
		bookmark.setId(getId());
		bookmark.setUserId(getUserId());
		bookmark.setDescription(getDescription());
		bookmark.setPosition(getPosition());
		bookmark.setTargetId(getTargetId());
		bookmark.setUserId(getUserId());
		bookmark.setType(getType());
		bookmark.setTitle(getTitle());
		bookmark.setFileType(getFileType());

		return bookmark;
	}

	public static WSBookmark fromBookmark(Bookmark bookmark) {
		WSBookmark wsBookmark = new WSBookmark();
		wsBookmark.setId(bookmark.getId());
		wsBookmark.setUserId(bookmark.getUserId());
		wsBookmark.setDescription(bookmark.getDescription());
		wsBookmark.setPosition(bookmark.getPosition());
		wsBookmark.setTargetId(bookmark.getTargetId());
		wsBookmark.setUserId(bookmark.getUserId());
		wsBookmark.setType(bookmark.getType());
		wsBookmark.setTitle(bookmark.getTitle());
		wsBookmark.setFileType(bookmark.getFileType());
		return wsBookmark;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public long getTargetId() {
		return targetId;
	}

	public void setTargetId(long targetId) {
		this.targetId = targetId;
	}

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

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getFileType() {
		return fileType;
	}

	public void setFileType(String fileType) {
		this.fileType = fileType;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}
}