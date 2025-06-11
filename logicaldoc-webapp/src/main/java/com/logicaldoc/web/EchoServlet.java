package com.logicaldoc.web;

import java.io.IOException;
import java.util.stream.Collectors;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;

/**
 * This servlet just prints the content it receives.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class EchoServlet extends HttpServlet {

	private static final long serialVersionUID = -6956612970433309888L;

	private static final Logger log = LoggerFactory.getLogger("console");

	/**
	 * Constructor of the object.
	 */
	public EchoServlet() {
		super();
	}

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		if (Context.get().getProperties().getBoolean("echo.enabled", false) && log.isInfoEnabled()) {
			log.info(getUrl(request));
			log.info(getContent(request));
		}
	}

	private String getContent(HttpServletRequest request) throws IOException {
		String content = "";
		if ("POST".equalsIgnoreCase(request.getMethod())) {
			content = request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
		}
		return content;
	}

	private String getUrl(HttpServletRequest request) {
		String scheme = request.getScheme(); // http
		String serverName = request.getServerName(); // hostname.com
		int serverPort = request.getServerPort(); // 80
		String contextPath = request.getContextPath(); // /mywebapp
		String servletPath = request.getServletPath(); // /servlet/MyServlet
		String pathInfo = request.getPathInfo(); // /a/b;c=123
		String queryString = request.getQueryString(); // d=789

		// Reconstruct original requesting URL
		StringBuilder url = new StringBuilder();
		url.append(scheme).append("://").append(serverName);

		if (serverPort != 80 && serverPort != 443) {
			url.append(":").append(serverPort);
		}

		url.append(contextPath).append(servletPath);

		if (pathInfo != null) {
			url.append(pathInfo);
		}
		if (queryString != null) {
			url.append("?").append(queryString);
		}
		return url.toString();
	}

}