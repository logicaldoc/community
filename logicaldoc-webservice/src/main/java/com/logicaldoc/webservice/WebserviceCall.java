package com.logicaldoc.webservice;

import com.logicaldoc.core.history.History;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * Represents a call to the webservice
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
@Entity
@Table(name = "ld_webservicecall")
@Cacheable
public class WebserviceCall extends History {

	private static final long serialVersionUID = 1L;

	public static final String SOAP = "soap";

	public static final String REST = "rest";

	public static final String ASPECT = "saveApiCall";

	/**
	 * The protocol used for the communication, can be WebserviceCall.SOAP or
	 * WebserviceCall.REST
	 */
	@Column(name = "ld_protocol")
	private String protocol = SOAP;

	public WebserviceCall() {
		setEvent(WebserviceCallEvent.CALL);
	}
	
	public void setEvent(WebserviceCallEvent event) {
		this.event = (event != null) ? event.toString() : null;
	}

	public WebserviceCallEvent getEventEnum() {
		if (event == null) return null;
		return WebserviceCallEvent.fromKey(event);
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((protocol == null) ? 0 : protocol.hashCode());
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
		WebserviceCall other = (WebserviceCall) obj;
		if (protocol == null) {
			if (other.protocol != null)
				return false;
		} else if (!protocol.equals(other.protocol))
			return false;
		return true;
	}
}