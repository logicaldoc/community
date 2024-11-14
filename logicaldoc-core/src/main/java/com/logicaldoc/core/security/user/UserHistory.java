package com.logicaldoc.core.security.user;

import javax.persistence.Table;

import com.logicaldoc.core.history.History;
import com.logicaldoc.core.security.Session;

/**
 * History entry due to an event on a user.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
@Table(name = "ld_user_history")
public class UserHistory extends History {

	private static final long serialVersionUID = 1L;

	private String author;

	public UserHistory() {
		super();
	}

	public UserHistory(Session session) {
		super();
		setSession(session);
	}
	
	public UserHistory(UserHistory source) {
		copyAttributesFrom(source);
		setAuthor(source.getAuthor());
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((author == null) ? 0 : author.hashCode());
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
		UserHistory other = (UserHistory) obj;
		if (author == null) {
			if (other.author != null)
				return false;
		} else if (!author.equals(other.author))
			return false;
		return true;
	}
}