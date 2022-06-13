package com.logicaldoc.web.util;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class VirtualHostHelper {

	private static final int HTTP_PORT_NUMBER = 80;

	private static final int HTTPS_PORT_NUMBER = 443;

	private static final Logger log = LoggerFactory.getLogger(VirtualHostHelper.class);

	private static final String X_FORWARDED_HOST = "x-forwarded-host";

	private static final String X_FORWARDED_PROTO = "x-forwarded-proto";

	private static final String X_FORWARDED_PORT = "x-forwarded-port";

	private static final String VH_HEADER = "logicaldoc-virtual-host";

	// Utility class.
	private VirtualHostHelper() {
	}

	private static HttpServletRequest getHttpServletRequest(ServletRequest request) {
		if (request instanceof HttpServletRequest) {
			return (HttpServletRequest) request;
		}
		return null;
	}

	/**
	 * Gets the name of the web application
	 * 
	 * @param request the HTTP request
	 * 
	 * @return WebApp name : ie : logicaldoc
	 */
	public static String getWebAppName(ServletRequest request) {
		HttpServletRequest httpRequest = getHttpServletRequest(request);
		if (httpRequest == null) {
			return "logicaldoc";
		} else {
			return httpRequest.getContextPath().replace("/", "");
		}
	}

	/**
	 * Gets the server's URL
	 * 
	 * @param request the HTTP request
	 * 
	 * @return Server URL as : protocol://serverName:port/
	 */
	public static String getServerURL(ServletRequest request) {
		return getServerURL(request, false);
	}

	private static String getServerUrl(String scheme, String serverName, int serverPort) {

		StringBuilder sbaseURL = new StringBuilder();
		sbaseURL.append(scheme);
		sbaseURL.append("://");
		sbaseURL.append(serverName);

		if (serverPort != 0) {
			log.debug("serverPort != 0; serverPort: " + serverPort);
			if (("http".equals(scheme) && serverPort != HTTP_PORT_NUMBER)
					|| ("https".equals(scheme) && serverPort != HTTPS_PORT_NUMBER)) {
				sbaseURL.append(':');
				sbaseURL.append(serverPort);
			}
		}
		sbaseURL.append('/');

		return sbaseURL.toString();
	}

	/**
	 * Gets the server's URL
	 * 
	 * @param request the HTTP request
	 * @param local if the url must be local
	 * 
	 * @return Server URL as : protocol://serverName:port/
	 */
	public static String getServerURL(ServletRequest request, boolean local) {

		String baseURL = null;
		HttpServletRequest httpRequest = getHttpServletRequest(request);

		if (httpRequest != null) {
			// Detect LogicalDOC specific header for VH
			String logicaldocVH = httpRequest.getHeader(VH_HEADER);
			if (!local && logicaldocVH != null && logicaldocVH.contains("http")) {
				log.debug("logicaldocVH.contains(http)");
				log.debug("logicaldocVH: " + logicaldocVH);
				baseURL = logicaldocVH;
			} else {
				// default values
				String serverName = httpRequest.getServerName();
				int serverPort = httpRequest.getServerPort();
				String scheme = httpRequest.getScheme();

				if (!local) {
					String forwardedPort = httpRequest.getHeader(X_FORWARDED_PORT);

					if (forwardedPort != null) {
						log.info("forwardedPort != null");
						try {
							serverPort = Integer.parseInt(forwardedPort);
							log.debug("forwardedPort != null; serverPort: " + serverPort);
						} catch (NumberFormatException e) {
							log.error("Unable to get forwarded port from header", e);
						}
					}

					String forwardedProto = httpRequest.getHeader(X_FORWARDED_PROTO);
					if (forwardedProto != null) {
						scheme = forwardedProto;
					}

					// Detect virtual hosting based in standard header
					String forwardedHost = httpRequest.getHeader(X_FORWARDED_HOST);
					if (forwardedHost != null) {
						log.debug("forwardedHost != null");
						if (forwardedHost.contains(":")) {
							serverName = forwardedHost.split(":")[0];
							serverPort = Integer.valueOf(forwardedHost.split(":")[1]);
							log.debug("forwardedHost contains ':', serverPort: " + serverPort);
						} else {
							log.debug("forwardedHost NOT contains ':', using fallback");
							serverName = forwardedHost;
							serverPort = HTTP_PORT_NUMBER; // fallback
						}
					}
				}

				baseURL = getServerUrl(scheme, serverName, serverPort);
			}
		}
		if (baseURL == null) {
			log.warn("Could not retrieve base url correctly");
		}
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
		String baseURL = null;
		String serverUrl = getServerURL(request, false);
		if (serverUrl != null) {
			String webAppName = getWebAppName(request);

			baseURL = StringUtils.isNotBlank(webAppName) ? serverUrl + webAppName + '/' : serverUrl;

		}
		return baseURL;
	}
}