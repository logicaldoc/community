package com.logicaldoc.core.security;

import com.logicaldoc.core.document.AbstractHistory;
import com.logicaldoc.core.folder.FolderHistory;

/**
 * History entry due to an event on a user.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class UserHistory extends AbstractHistory {

	// Events on users
	public final static String EVENT_USER_LOGIN = "event.user.login";

	public final static String EVENT_USER_LOGIN_FAILED = "event.user.login.failed";

	public final static String EVENT_USER_LOGOUT = "event.user.logout";

	public final static String EVENT_USER_TIMEOUT = "event.user.timeout";

	public final static String EVENT_USER_PASSWORDCHANGED = "event.user.passwordchanged";
	
	public final static String EVENT_USER_2FACHANGED = "event.user.2fachanged";

	public final static String EVENT_USER_DELETED = "event.user.deleted";
	
	public final static String EVENT_FILE_CONVERSION = "event.user.fileconversion";
	
	@Override
	public Object clone() throws CloneNotSupportedException {
		UserHistory history = new UserHistory();
		history.setDate(getDate());
		history.setDocId(getDocId());
		history.setFolderId(getFolderId());
		history.setUser(getUser());
		history.setEvent(getEvent());
		history.setComment(getComment());
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

		return history;
	}
}