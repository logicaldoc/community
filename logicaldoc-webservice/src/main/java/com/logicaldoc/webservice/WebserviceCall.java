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

	public static final String SOAP = "soap";

	public static final String REST = "rest";

	public static final String ASPECT = "saveApiCall";

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
		copyAttributesFrom(source);

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