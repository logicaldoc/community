package com.logicaldoc.webservice.doc.model;

import java.util.ArrayList;
import java.util.List;

/**
 * it correspond to a web service class
 */
public class WebServiceStubSet {

	private Class<?> webServiceClass;

	private List<WebMethodStubSet> methodStubs = new ArrayList<WebMethodStubSet>();

	private String release;

	public List<WebMethodStubSet> getMethodStubs() {
		return methodStubs;
	}

	public void addMethodStub(WebMethodStubSet methodStub) {
		methodStubs.add(methodStub);
	}

	public Class<?> getWebServiceClass() {
		return webServiceClass;
	}

	public void setWebServiceClass(Class<?> webServiceClass) {
		this.webServiceClass = webServiceClass;
	}

	public String getRelease() {
		return release;
	}

	public void setRelease(String release) {
		this.release = release;
	}

}
