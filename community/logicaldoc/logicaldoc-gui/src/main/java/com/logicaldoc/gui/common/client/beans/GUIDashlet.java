package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Setting for the dashlet visualization.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.6
 */
public class GUIDashlet implements Serializable {

	private static final long serialVersionUID = 1L;

	private int id;

	private int column;

	private int row;

	private int index;

	public GUIDashlet() {
	}

	public GUIDashlet(int id, int column, int row, int index) {
		super();
		this.id = id;
		this.column = column;
		this.row = row;
		this.index = index;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public int getColumn() {
		return column;
	}

	public void setColumn(int column) {
		this.column = column;
	}

	public int getRow() {
		return row;
	}

	public void setRow(int row) {
		this.row = row;
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}
}