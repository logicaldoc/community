package com.logicaldoc.web;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionContext;

/**
 * A servlet session useful for doing tests, you can easily setup the map of
 * attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 *
 */
public class MockServletSession implements HttpSession {

	private Map<String, Object> attributes = new HashMap<>();

	public Map<String, Object> getAttributes() {
		return attributes;
	}

	public void setAttributes(Map<String, Object> attributes) {
		this.attributes = attributes;
	}

	public MockServletSession() {

	}

	@Override
	public Object getAttribute(String name) {
		return attributes.get(name);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Enumeration getAttributeNames() {
		return null;
	}

	@Override
	public long getCreationTime() {

		return 0;
	}

	@Override
	public String getId() {

		return null;
	}

	@Override
	public long getLastAccessedTime() {

		return 0;
	}

	@Override
	public int getMaxInactiveInterval() {

		return 0;
	}

	@Override
	public ServletContext getServletContext() {

		return null;
	}

	@SuppressWarnings("deprecation")
	@Override
	public HttpSessionContext getSessionContext() {

		return null;
	}

	@Override
	public Object getValue(String arg0) {

		return null;
	}

	@Override
	public String[] getValueNames() {

		return null;
	}

	@Override
	public void invalidate() {

	}

	@Override
	public boolean isNew() {

		return false;
	}

	@Override
	public void putValue(String arg0, Object arg1) {

	}

	@Override
	public void removeAttribute(String arg0) {

	}

	@Override
	public void removeValue(String arg0) {

	}

	@Override
	public void setAttribute(String name, Object value) {
		attributes.put(name, value);
	}

	@Override
	public void setMaxInactiveInterval(int arg0) {

	}

}
