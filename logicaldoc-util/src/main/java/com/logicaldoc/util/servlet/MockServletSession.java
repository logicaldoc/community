package com.logicaldoc.util.servlet;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpSession;

/**
 * A servlet session useful for doing tests, you can easily setup the map of
 * attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
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
		super();
	}

	@Override
	public Object getAttribute(String name) {
		return attributes.get(name);
	}

	@Override
	public Enumeration<String> getAttributeNames() {
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

	@Override
	public void invalidate() {
		// Nothing to do
	}

	@Override
	public boolean isNew() {
		return false;
	}

	@Override
	public void removeAttribute(String arg0) {
		// Nothing to do
	}

	@Override
	public void setAttribute(String name, Object value) {
		attributes.put(name, value);
	}

	@Override
	public void setMaxInactiveInterval(int arg0) {
		// Nothing to do
	}
}