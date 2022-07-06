package com.logicaldoc.gui.common.client.util;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.http.client.URL;

/**
 * Collects various informations about the current request.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class RequestInfo {
	private String hash;

	private String host;

	private String hostName;

	private String href;

	private String path;

	private String port;

	private String protocol;

	private String queryString;

	private HashMap<String, String> paramMap;

	public String getHash() {
		return hash;
	}

	public String getHost() {
		return host;
	}

	public String getHostName() {
		return hostName;
	}

	public String getHref() {
		return href;
	}

	public String getPath() {
		return path;
	}

	public String getPort() {
		return port;
	}

	public String getProtocol() {
		return protocol;
	}

	public String getQueryString() {
		return queryString;
	}

	protected void setHash(String hash) {
		this.hash = hash;
	}

	protected void setHost(String host) {
		this.host = host;
	}

	protected void setHostName(String hostName) {
		this.hostName = hostName;
	}

	protected void setHref(String href) {
		this.href = href;
	}

	protected void setPath(String path) {
		this.path = path;
	}

	protected void setPort(String port) {
		this.port = port;
	}

	protected void setProtocol(String protocol) {
		this.protocol = protocol.toLowerCase();
		if (this.protocol.endsWith(":"))
			this.protocol = this.protocol.replace(":", "");
	}

	protected void setQueryString(String queryString) {
		this.queryString = queryString;
		paramMap = new HashMap<String, String>();

		if (queryString != null && queryString.length() > 1) {
			String qs = queryString.substring(1);
			String[] kvPairs = qs.split("&");
			for (int i = 0; i < kvPairs.length; i++) {
				String[] kv = kvPairs[i].split("=");
				if (kv.length > 1) {
					paramMap.put(kv[0], URL.decodeComponent(kv[1]));
				} else {
					paramMap.put(kv[0], "");
				}
			}
		}
	}

	public String getParameter(String name) {
		return (String) paramMap.get(name);
	}

	public Map<String, String> getParameterMap() {
		return paramMap;
	}

	public boolean isSecure() {
		return "https".equals(protocol.toLowerCase());
	}
}
