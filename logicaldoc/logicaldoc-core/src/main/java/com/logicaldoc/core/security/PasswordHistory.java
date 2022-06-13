package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * This class represent an old password of a user
 * 
 * @author Marco Meschieri
 * 
 * @version 8.6.1
 */
public class PasswordHistory extends PersistentObject implements Serializable, Comparable<PasswordHistory> {

	private static final long serialVersionUID = 1L;

	private long userId;

	private String password = "";

	private Date date =new Date();

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	@Override
	public int compareTo(PasswordHistory other) {
		if (other.userId == userId)
			return this.date.compareTo(other.date);
		else
			return Long.valueOf(userId).compareTo(Long.valueOf(other.userId));
	}
}