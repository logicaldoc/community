package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * Representation of a single document handled by the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class GUIDocumentNote implements Serializable {
	private static final long serialVersionUID = 1L;

	private long id;

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

	public long getId() {
		return id;
	}

	public long getDocId() {
		return docId;
	}

	public String getFileName() {
		return fileName;
	}

	public long getUserId() {
		return userId;
	}

	public String getUsername() {
		return username;
	}

	public Date getDate() {
		return date;
	}

	public String getMessage() {
		return message;
	}

	public int getPage() {
		return page;
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public int getOpacity() {
		return opacity;
	}

	public String getColor() {
		return color;
	}

	public void setOpacity(int opacity) {
		this.opacity = opacity;
	}

	public void setColor(String color) {
		this.color = color;
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
}