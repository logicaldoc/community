package com.logicaldoc.webservice.model;

import java.util.Date;

import com.logicaldoc.core.document.Rating;
import com.logicaldoc.webservice.doc.WSDoc;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Web Service Rating.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
@XmlType(name = "WSRating")
public class WSRating {

	@WSDoc(description = "the referenced document")
	private long docId;

	@WSDoc(description = "id of the user that rated the document")
	private long userId;

	@WSDoc(description = "name of the user that rated the document")
	private String username;

	private Date date;

	private int vote = 0;

	public Rating toRating() {
		Rating rating = new Rating();
		rating.setLastModified(getDate());
		rating.setDocId(getDocId());
		rating.setVote(getVote());
		rating.setUserId(getUserId());
		rating.setUsername(getUsername());
		return rating;
	}

	public static WSRating fromRating(Rating rating) {
		WSRating wsRating = new WSRating();
		wsRating.setDate(rating.getLastModified());
		wsRating.setDocId(rating.getDocId());
		wsRating.setUserId(rating.getUserId());
		wsRating.setUsername(rating.getUsername());
		wsRating.setVote(rating.getVote());
		return wsRating;
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

	public int getVote() {
		return vote;
	}

	public void setVote(int vote) {
		this.vote = vote;
	}
}