package com.logicaldoc.core.security.user;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.logicaldoc.core.PersistentObject;

/**
 * This class represent an old password of a user
 * 
 * @author Marco Meschieri
 * 
 * @version 8.6.1
 */
@Entity
@Table(name = "ld_password_history")
@Cacheable
public class PasswordHistory extends PersistentObject implements Serializable, Comparable<PasswordHistory> {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_userid", nullable = false) 
	private long userId;

	@Column(name = "ld_date", columnDefinition = "DATETIME(3)")
	private Date date = new Date();
	
	@Column(name = "ld_password")
	private String password = "";

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