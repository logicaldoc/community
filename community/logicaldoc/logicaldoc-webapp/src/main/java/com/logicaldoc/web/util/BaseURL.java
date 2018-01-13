package com.logicaldoc.web.util;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BaseURL {

	private static final Logger log = LoggerFactory.getLogger(BaseURL.class);

	private BaseURL() {
	}

	/**
	 * @return Server URL as : protocol://serverName:port/
	 */
	public static String getServerURL(ServletRequest request, boolean local) {
		return VirtualHostHelper.getServerURL(request, local);
	}

	/**
	 * @return WebApp name : ie : logicaldoc
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
