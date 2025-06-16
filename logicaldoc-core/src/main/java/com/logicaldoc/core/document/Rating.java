package com.logicaldoc.core.document;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;

/**
 * A rating over a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@Entity
@Table(name = "ld_rating")
@Cacheable
public class Rating extends PersistentObject {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_docid", nullable = false)
	private long docId;

	@Column(name = "ld_userid", nullable = false)
	private long userId;

	@Column(name = "ld_username", length = 255)
	private String username;
	
	@Column(name = "ld_vote", nullable = false)
	private int vote = 0;

	@Transient
	private Integer count;

	@Transient
	private Float average;

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (int) (docId ^ (docId >>> 32));
		result = prime * result + (int) (userId ^ (userId >>> 32));
		result = prime * result + vote;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Rating other = (Rating) obj;
		if (docId != other.docId)
			return false;
		if (userId != other.userId)
			return false;
		return vote == other.vote;
	}
}