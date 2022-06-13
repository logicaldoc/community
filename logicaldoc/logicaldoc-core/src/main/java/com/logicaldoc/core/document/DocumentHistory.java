package com.logicaldoc.core.document;

import com.logicaldoc.core.History;

/**
 * Registers an event on folder or document
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @author Marco Meschieri - LogicalDOC
 */
public class DocumentHistory extends History {

	@Override
	public DocumentHistory clone() {
		DocumentHistory history = new DocumentHistory();
		history.setTenantId(getTenantId());
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
		history.setFile(getFile());
		history.setTenant(getTenant());
		history.setNotifyEvent(isNotifyEvent());
		history.setIp(getIp());
		history.setDevice(getDevice());
		history.setGeolocation(getGeolocation());
		history.setFileSize(getFileSize());
		return history;
	}
}