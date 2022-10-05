package com.logicaldoc.core.folder;

import com.logicaldoc.core.History;

/**
 * History entry due to an event on a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FolderHistory extends History {

	private static final long serialVersionUID = 1L;

	public FolderHistory() {

	}

	public FolderHistory( FolderHistory source) {
		setDate(source.getDate());
		setDocId(source.getDocId());
		setFolderId(source.getFolderId());
		setUser(source.getUser());
		setEvent(source.getEvent());
		setComment(source.getComment());
		setReason(source.getReason());
		setFileVersion(source.getFileVersion());
		setVersion(source.getVersion());
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
		setIp(source.getIp());
		setDevice(source.getDevice());
		setGeolocation(source.getGeolocation());
		setFileSize(source.getFileSize());
	}
}