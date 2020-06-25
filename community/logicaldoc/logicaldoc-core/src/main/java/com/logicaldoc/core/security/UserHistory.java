package com.logicaldoc.core.security;

import com.logicaldoc.core.History;

/**
 * History entry due to an event on a user.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class UserHistory extends History {

	public String author;
	
	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		UserHistory history = new UserHistory();
		history.setDate(getDate());
		history.setDocId(getDocId());
		history.setFolderId(getFolderId());
		history.setUser(getUser());
		history.setEvent(getEvent());
		history.setComment(getComment());
		history.setReason(getReason());
		history.setVersion(getVersion());
		history.setPath(getPath());
		history.setPathOld(getPathOld());
		history.setNotified(getNotified());
		history.setSessionId(getSessionId());
		history.setIsNew(getIsNew());
		history.setFilename(getFilename());
		history.setFilenameOld(getFilenameOld());
		history.setUserId(getUserId());
		history.setUsername(getUsername());
		history.setUserLogin(getUserLogin());
		history.setNotifyEvent(isNotifyEvent());
		history.setIp(getIp());
		history.setAuthor(getAuthor());

		return history;
	}
}