package com.logicaldoc.core.security;

import com.logicaldoc.core.History;

/**
 * History entry due to an event on a user.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class UserHistory extends History {

	private static final long serialVersionUID = 1L;
	
	public String author;
	
	public UserHistory() {
		super();
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
}