package com.logicaldoc.util.servlet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import jakarta.servlet.AsyncContext;
import jakarta.servlet.DispatcherType;
import jakarta.servlet.ReadListener;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletConnection;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.HttpUpgradeHandler;
import jakarta.servlet.http.Part;

/**
 * A servlet created for tests
 * 
 * @author Francesco Bignardi
 * @since 8.9
 */
public class MockServletRequest implements HttpServletRequest {

	private static final String CONTENT_LENGTH = "Content-Length";

	private HttpSession session;

	private String code;

	private String pathInfo = "/";

	private String method = "GET";

	private String contextPath = "";

	private String requestURI;

	private String body;

	private InputStream payload;

	private Map<String, String> headers = new HashMap<>();

	private Map<String, String[]> parameters = new HashMap<>();

	private Map<String, Object> attributes = new HashMap<>();

	public MockServletRequest() {
		super();
		setUserAgent("Mozilla");
	}

	public MockServletRequest(HttpSession session) {
		this();
		this.session = session;
	}

	public MockServletRequest(HttpSession session, String userAgent) {
		super();
		this.session = session;
		setUserAgent(userAgent);
	}

	public void setPathInfo(String pathInfo) {
		this.pathInfo = pathInfo;
	}

	public void setParameters(Map<String, String[]> parameters) {
		this.parameters = parameters;
	}

	public void setParameter(String name, String value) {
		parameters.put(name, new String[] { value });
	}

	public String getUserAgent() {
		return headers.get("User-Agent");
	}

	public void setUserAgent(String userAgent) {
		headers.put("User-Agent", userAgent);
	}

	public void setContentType(String contentType) {
		headers.put("Content-Type", contentType);
	}

	public void setSession(HttpSession session) {
		this.session = session;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	@Override
	public Object getAttribute(String arg0) {
		return null;
	}

	@Override
	public Enumeration<String> getAttributeNames() {
		return null;
	}

	@Override
	public String getCharacterEncoding() {
		return null;
	}

	@Override
	public int getContentLength() {
		return headers.get(CONTENT_LENGTH) != null ? Integer.parseInt(headers.get(CONTENT_LENGTH)) : 0;
	}

	public void setContentLength(int length) {
		headers.put(CONTENT_LENGTH, Integer.toString(length));
	}

	@Override
	public String getContentType() {
		return getHeader("Content-Type");
	}

	@Override
	public ServletInputStream getInputStream() throws IOException {
		if (body != null) {
			return new BytesServletInputStream(body.getBytes(StandardCharsets.UTF_8));
		} else if (payload != null) {
			return new ServletInputStream() {
				public int read() throws IOException {
					return payload.read();
				}

				@Override
				public boolean isFinished() {
					return false;
				}

				@Override
				public boolean isReady() {
					return false;
				}

				@Override
				public void setReadListener(ReadListener arg0) {
					// Do nothing
				}
			};
		}

		return null;
	}

	@Override
	public String getLocalAddr() {
		return null;
	}

	@Override
	public String getLocalName() {
		return null;
	}

	@Override
	public int getLocalPort() {
		return 0;
	}

	@Override
	public Locale getLocale() {
		return null;
	}

	@Override
	public Enumeration<Locale> getLocales() {
		return null;
	}

	@Override
	public String getParameter(String param) {
		return parameters.get(param) != null ? parameters.get(param)[0] : null;
	}

	@Override
	public Enumeration<String> getParameterNames() {
		return Collections.enumeration(parameters.keySet());
	}

	@Override
	public String[] getParameterValues(String param) {
		return parameters.get(param);
	}

	@Override
	public String getProtocol() {
		return null;
	}

	@Override
	public BufferedReader getReader() throws IOException {
		if (body != null)
			return new BufferedReader(new StringReader(body));
		else
			return null;
	}

	@Override
	public String getRemoteAddr() {

		return null;
	}

	@Override
	public String getRemoteHost() {

		return null;
	}

	@Override
	public int getRemotePort() {

		return 0;
	}

	@Override
	public RequestDispatcher getRequestDispatcher(String arg0) {

		return null;
	}

	@Override
	public String getScheme() {

		return null;
	}

	@Override
	public String getServerName() {
		return null;
	}

	@Override
	public int getServerPort() {
		return 0;
	}

	@Override
	public boolean isSecure() {
		return false;
	}

	@Override
	public void removeAttribute(String attribute) {
		attributes.remove(attribute);
	}

	@Override
	public void setAttribute(String attribute, Object value) {
		attributes.put(attribute, value);
	}

	@Override
	public void setCharacterEncoding(String encofing) throws UnsupportedEncodingException {
		// Nothing to do
	}

	@Override
	public String getAuthType() {
		return null;
	}

	@Override
	public String getContextPath() {
		return contextPath;
	}

	@Override
	public Cookie[] getCookies() {
		return new Cookie[0];
	}

	@Override
	public long getDateHeader(String arg0) {
		return 0;
	}

	@Override
	public String getHeader(String header) {
		return headers.get(header);
	}

	@Override
	public Enumeration<String> getHeaderNames() {
		return Collections.enumeration(headers.keySet());
	}

	@Override
	public Enumeration<String> getHeaders(String arg0) {
		ArrayList<String> result = new ArrayList<String>();
		String myhead = headers.get(arg0);
		if (myhead != null)
			result.add(myhead);

		return Collections.enumeration(result);
	}

	@Override
	public int getIntHeader(String arg0) {
		return 0;
	}

	@Override
	public String getMethod() {
		return method;
	}

	@Override
	public String getPathInfo() {
		return pathInfo;
	}

	@Override
	public String getPathTranslated() {
		return null;
	}

	@Override
	public String getQueryString() {
		return null;
	}

	@Override
	public String getRemoteUser() {
		return null;
	}

	@Override
	public String getRequestURI() {
		return requestURI;
	}

	@Override
	public StringBuffer getRequestURL() {
		return null;
	}

	@Override
	public String getRequestedSessionId() {

		return null;
	}

	@Override
	public String getServletPath() {

		return null;
	}

	@Override
	public HttpSession getSession() {
		return session;
	}

	@Override
	public HttpSession getSession(boolean arg0) {
		return session;
	}

	@Override
	public Principal getUserPrincipal() {
		return null;
	}

	@Override
	public boolean isRequestedSessionIdFromCookie() {
		return false;
	}

	@Override
	public boolean isRequestedSessionIdFromURL() {
		return false;
	}

	@Override
	public boolean isRequestedSessionIdValid() {
		return false;
	}

	@Override
	public boolean isUserInRole(String arg0) {
		return false;
	}

	public void setMethod(String method) {
		this.method = method;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
		setContentLength(body.getBytes(StandardCharsets.UTF_8).length);
	}

	public void setContextPath(String contextPath) {
		this.contextPath = contextPath;
	}

	public void setRequestURI(String requestURI) {
		this.requestURI = requestURI;
	}

	public Map<String, String> getHeaders() {
		return headers;
	}

	public void setHeader(String name, String value) {
		this.headers.put(name, value);
	}

	public void removeHeader(String name) {
		this.headers.remove(name);
	}

	public void setHeaders(Map<String, String> headers) {
		this.headers = headers;
	}

	public InputStream getPayload() {
		return payload;
	}

	public void setPayload(InputStream payload) {
		this.payload = payload;
	}

	@Override
	public AsyncContext getAsyncContext() {
		return null;
	}

	@Override
	public long getContentLengthLong() {
		return 0;
	}

	@Override
	public DispatcherType getDispatcherType() {
		return null;
	}

	@Override
	public ServletContext getServletContext() {
		return null;
	}

	@Override
	public boolean isAsyncStarted() {
		return false;
	}

	@Override
	public boolean isAsyncSupported() {
		return false;
	}

	@Override
	public AsyncContext startAsync() throws IllegalStateException {
		return null;
	}

	@Override
	public AsyncContext startAsync(ServletRequest arg0, ServletResponse arg1) throws IllegalStateException {
		return null;
	}

	@Override
	public boolean authenticate(HttpServletResponse arg0) throws IOException, ServletException {
		return false;
	}

	@Override
	public String changeSessionId() {
		return null;
	}

	@Override
	public Part getPart(String arg0) throws IOException, ServletException {
		return null;
	}

	@Override
	public Collection<Part> getParts() throws IOException, ServletException {
		return new ArrayList<>();
	}

	@Override
	public void login(String arg0, String arg1) throws ServletException {
		throw new UnsupportedOperationException();
	}

	@Override
	public void logout() throws ServletException {
		throw new UnsupportedOperationException();
	}

	@Override
	public <T extends HttpUpgradeHandler> T upgrade(Class<T> arg0) throws IOException, ServletException {
		return null;
	}

	@Override
	public String getProtocolRequestId() {
		return null;
	}

	@Override
	public String getRequestId() {
		return null;
	}

	@Override
	public ServletConnection getServletConnection() {
		return null;
	}

	@Override
	public Map<String, String[]> getParameterMap() {
		return parameters;
	}
}