package com.logicaldoc.core.document;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * A note over a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class DocumentNote extends PersistentObject implements Cloneable {

	private long docId;

	private String fileVersion;

	private String fileName;

	private long userId;

	private String username;

	private Date date = new Date();

	private String message;

	private int page = 0;

	private int opacity = 80;

	private String color = "#FFFF88";

	private double left = 0.5;

	private double top = 0.5;

	private double width = 0.15;

	private double height = 0.10;

	public DocumentNote() {
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public int getOpacity() {
		return opacity;
	}

	public String getColor() {
		return color;
	}

	public double getLeft() {
		return left;
	}

	public double getTop() {
		return top;
	}

	public double getWidth() {
		return width;
	}

	public double getHeight() {
		return height;
	}

	public void setOpacity(int opacity) {
		this.opacity = opacity;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public void setLeft(double left) {
		this.left = left;
	}

	public void setTop(double top) {
		this.top = top;
	}

	public void setWidth(double width) {
		this.width = width;
	}

	public void setHeight(double height) {
		this.height = height;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		DocumentNote note=new DocumentNote();
		note.setId(getId());
		note.setDate(getDate());
		note.setDocId(getDocId());
		note.setMessage(getMessage());
		note.setUserId(getUserId());
		note.setUsername(getUsername());
		note.setPage(getPage());
		note.setOpacity(getOpacity());
		note.setColor(getColor());
		note.setTop(getTop());
		note.setLeft(getLeft());
		note.setWidth(getWidth());
		note.setHeight(getHeight());
		note.setFileVersion(getFileVersion());
		note.setFileName(getFileName());
		return note;
	}
}