package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * A document's rating.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class GUIRating implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private long docId;

	private long userId;

	private String username;
	
	private int vote;

	private Integer count;

	private Float average;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
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

	public int getVote() {
		return vote;
	}

	public void setVote(int vote) {
		this.vote = vote;
	}

	public Integer getCount() {
		return count;
	}

	public void setCount(Integer count) {
		this.count = count;
	}

	public Float getAverage() {
		return average;
	}

	public void setAverage(Float average) {
		this.average = average;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}
}
