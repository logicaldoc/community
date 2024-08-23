package com.logicaldoc.core.security.user;

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

	private Date date = new Date();

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
		if(equals(other))
			return 0;
		if (other.userId == userId)
			return this.date.compareTo(other.date);
		else
			return Long.compare(userId, other.userId);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((password == null) ? 0 : password.hashCode());
		result = prime * result + (int) (userId ^ (userId >>> 32));
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
		PasswordHistory other = (PasswordHistory) obj;
		if (password == null) {
			if (other.password != null)
				return false;
		} else if (!password.equals(other.password))
			return false;
		return userId == other.userId;
	}
}