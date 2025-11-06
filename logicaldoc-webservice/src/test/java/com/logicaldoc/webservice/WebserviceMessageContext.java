package com.logicaldoc.webservice;

import org.apache.cxf.jaxrs.ext.MessageContext;

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.Request;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.core.UriInfo;
import jakarta.ws.rs.ext.Providers;

public class WebserviceMessageContext implements MessageContext {

	public WebserviceMessageContext() {
		
	}

	@Override
	public Object get(Object arg0) {
		
		return null;
	}

	@Override
	public <T> T getContent(Class<T> arg0) {
		
		return null;
	}

	@Override
	public <T> T getContext(Class<T> arg0) {
		
		return null;
	}

	@Override
	public Object getContextualProperty(Object arg0) {
		
		return null;
	}

	@Override
	public HttpHeaders getHttpHeaders() {
		
		return null;
	}

	@Override
	public HttpServletRequest getHttpServletRequest() {
		
		return null;
	}

	@Override
	public HttpServletResponse getHttpServletResponse() {
		
		return null;
	}

	@Override
	public Providers getProviders() {
		
		return null;
	}

	@Override
	public Request getRequest() {
		
		return null;
	}

	@Override
	public <T, E> T getResolver(Class<T> arg0, Class<E> arg1) {
		
		return null;
	}

	@Override
	public SecurityContext getSecurityContext() {
		
		return null;
	}

	@Override
	public ServletConfig getServletConfig() {
		
		return null;
	}

	@Override
	public ServletContext getServletContext() {
		
		return null;
	}

	@Override
	public UriInfo getUriInfo() {
		
		return null;
	}

	@Override
	public void put(Object arg0, Object arg1) {
		

	}

}
