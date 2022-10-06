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
	 * The protocol used for the communication, can be WebserviceCall.SOAP or
	 * WebserviceCall.REST
	 */
	private String protocol = SOAP;

	public WebserviceCall() {
		setEvent("event.webservice.call");
	}

	public WebserviceCall(WebserviceCall source) {
		this();

		this.protocol = source.protocol;

		setTenantId(source.getTenantId());
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
		setFile(source.getFile());
		setTenant(source.getTenant());
		setNotifyEvent(isNotifyEvent());
		setIp(source.getIp());
		setDevice(source.getDevice());
		setGeolocation(source.getGeolocation());
		setFileSize(source.getFileSize());
		setProtocol(source.getProtocol());
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
	public String toString() {
		return "WebserviceCall [uri=" + getUri() + ", date=" + getDate() + "]";
	}
}