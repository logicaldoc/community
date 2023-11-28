package com.logicaldoc.webservice;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.ext.Providers;

import org.apache.cxf.jaxrs.ext.MessageContext;

public class WebserviceMessageContext implements MessageContext {

	public WebserviceMessageContext() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public Object get(Object arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T getContent(Class<T> arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T getContext(Class<T> arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object getContextualProperty(Object arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public HttpHeaders getHttpHeaders() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public HttpServletRequest getHttpServletRequest() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public HttpServletResponse getHttpServletResponse() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Providers getProviders() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Request getRequest() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T, E> T getResolver(Class<T> arg0, Class<E> arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public SecurityContext getSecurityContext() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ServletConfig getServletConfig() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ServletContext getServletContext() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public UriInfo getUriInfo() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void put(Object arg0, Object arg1) {
		// TODO Auto-generated method stub

	}

}
