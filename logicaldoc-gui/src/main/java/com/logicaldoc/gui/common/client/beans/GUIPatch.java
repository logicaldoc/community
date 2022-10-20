package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * GUI representation of a patch.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
public class GUIPatch implements Serializable {

	private static final long serialVersionUID = 1L;

	private String id;

	private String name;

	private String file;

	private Date date;

	private String description;

	private long size;

	private boolean installed = false;

	private boolean restart = true;

	private int rating = 0;

	public String getId() {
		return id;
	}

	public Date getDate() {
		return date;
	}

	public String getDescription() {
		return description;
	}

	public long getSize() {
		return size;
	}

	public boolean isInstalled() {
		return installed;
	}

	public void setId(String id) {
		this.id = id;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public void setInstalled(boolean installed) {
		this.installed = installed;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getFile() {
		return file;
	}

	public void setFile(String file) {
		this.file = file;
	}

	@Override
	public String toString() {
		return name;
	}

	public boolean isRestart() {
		return restart;
	}

	public void setRestart(boolean restart) {
		this.restart = restart;
	}

	public int getRating() {
		return rating;
	}

	public void setRating(int rating) {
		this.rating = rating;
	}

	public static String getColor(int rating) {
		if (rating == 2)
			return "orange";
		else if (rating == 3)
			return "red";
		else
			return "";
	}

	public String getColor() {
		return getColor(rating);
	}
}