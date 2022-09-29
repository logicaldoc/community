package com.logicaldoc.webservice;

import com.logicaldoc.core.History;

/**
 * Represents a call to the webservice
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WebserviceCall extends History {

	private static final long serialVersionUID = 1L;

	public static String SOAP = "soap";

	public static String REST = "rest";

	public static String ASPECT = "saveApiCall";
	
	/**
	 * The protocol used for the communication, can be
	 * WebserviceCall.SOAP or WebserviceCall.REST
	 */
	private String protocol = SOAP;

	public WebserviceCall() {
		setEvent("event.webservice.call");
	}

	public String getPayload() {
		return getComment();
	}

	public void setPayload(String payload) {
		setComment(payload);
	}
	
	public String getUri() {
		return getPath();
	}

	public void setUri(String uri) {
		setPath(uri);
	}

	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	@Override
	public Object clone() {
		WebserviceCall history = new WebserviceCall();
		history.setTenantId(getTenantId());
		history.setDate(getDate());
		history.setDocId(getDocId());
		history.setFolderId(getFolderId());
		history.setUser(getUser());
		history.setEvent(getEvent());
		history.setComment(getComment());
		history.setReason(getReason());
		history.setVersion(getVersion());
		history.setFileVersion(getFileVersion());
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
		history.setProtocol(getProtocol());
		return history;
	}

	@Override
	public String toString() {
		return "WebserviceCall [uri=" + getUri() + ", date=" + getDate() + "]";
	}
}