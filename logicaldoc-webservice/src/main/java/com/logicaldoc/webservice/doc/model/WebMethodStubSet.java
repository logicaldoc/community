package com.logicaldoc.webservice.doc.model;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.webservice.doc.service.StubTypeTreeRepository;

/**
 * it correspond to a method in a web service
 */
public class WebMethodStubSet {

	private String methodName;

	private String description;

	private List<Stub> requestStubs = new ArrayList<Stub>();

	private Stub responseStub;

	private StubTypeTreeRepository stubTypeTreeRepository = new StubTypeTreeRepository();

	public String getMethodName() {
		return methodName;
	}

	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}

	public Stub getResponseStub() {
		return responseStub;
	}

	public void setResponseStub(Stub responseStub) {
		this.responseStub = responseStub;
	}

	public List<Stub> getRequestStubs() {
		return requestStubs;
	}

	public void addRequetStub(Stub stub) {
		requestStubs.add(stub);
	}

	public StubTypeTreeRepository getStubTypeTreeRepository() {
		return stubTypeTreeRepository;
	}

	public boolean isInheritanceInvolved() {
		return !stubTypeTreeRepository.isEmpty();
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}