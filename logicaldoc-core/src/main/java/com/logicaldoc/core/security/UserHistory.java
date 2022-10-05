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
	}

	public UserHistory(UserHistory source) {
		setDate(source.getDate());
		setDocId(source.getDocId());
		setFolderId(source.getFolderId());
		setUser(source.getUser());
		setEvent(source.getEvent());
		setComment(source.getComment());
		setReason(source.getReason());
		setVersion(source.getVersion());
		setFileVersion(source.getFileVersion());
		setPath(source.getPath());
		setPathOld(source.getPathOld());
		setNotified(source.getNotified());
		setSessionId(source.getSessionId());
		setIsNew(source.getIsNew());
		setFilename(source.getFilename());
		setFilenameOld(source.getFilenameOld());
		setUserId(source.getUserId());
		setUsername(source.getUsername());
		setUserLogin(source.getUserLogin());
		setNotifyEvent(isNotifyEvent());
		setAuthor(source.getAuthor());
		setIp(source.getIp());
		setDevice(source.getDevice());
		setGeolocation(source.getGeolocation());
		setFileSize(source.getFileSize());
	}
	
	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}
}