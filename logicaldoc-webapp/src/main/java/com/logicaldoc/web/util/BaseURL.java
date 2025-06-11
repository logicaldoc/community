package com.logicaldoc.web.util;

import jakarta.servlet.ServletRequest;
import jakarta.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BaseURL {

	private static final Logger log = LoggerFactory.getLogger(BaseURL.class);

	private BaseURL() {
	}

	/**
	 * Gets the server's URL
	 * 
	 * @param request the HTTP request
	 * @param local if the URL must be local
	 * 
	 * @return Server URL as protocol://serverName:port/
	 */
	public static String getServerURL(ServletRequest request, boolean local) {
		return VirtualHostHelper.getServerURL(request, local);
	}

	/**
	 * Gets the name od the application
	 * 
	 * @param request the HTTP request
	 * 
	 * @return WebApp name, i.e.: logicaldoc
	 */
	public static String getWebAppName(HttpServletRequest request) {
		String baseURL = request.getContextPath();

		if (baseURL == null) {
			return "logicaldoc";
		}

		baseURL = baseURL.replace("/", "");
		return baseURL;
	}

	/**
	 * Gets the base URL
	 * 
	 * @param request the HTTP request
	 * 
	 * @return base URL as protocol://serverName:port/webappName/
	 */
	public static String getBaseURL(ServletRequest request) {
		return VirtualHostHelper.getBaseURL(request);
	}

	public static String getLocalBaseURL(HttpServletRequest request) {
		String localURL = null;
		String serverUrl = getServerURL(request, true);
		if (serverUrl != null) {
			localURL = serverUrl + getWebAppName(request);
			if (!localURL.endsWith("/"))
				localURL = localURL + "/";
		}

		if (localURL == null) {
			log.error("Could not retrieve loacl url correctly");
		}
		return localURL;
	}
}