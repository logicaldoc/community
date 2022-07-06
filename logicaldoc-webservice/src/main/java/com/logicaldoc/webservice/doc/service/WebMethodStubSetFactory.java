package com.logicaldoc.webservice.doc.service;

import static com.logicaldoc.webservice.doc.service.JavaLanguageVariableFactory.createVariableFromMethodReturn;
import static com.logicaldoc.webservice.doc.service.JavaLanguageVariableFactory.createVariablesFromMethodParamaters;

import java.lang.reflect.Method;
import java.util.List;

import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.doc.model.JavaLanguageVariable;
import com.logicaldoc.webservice.doc.model.Stub;
import com.logicaldoc.webservice.doc.model.WebMethodStubSet;

/**
 * 
 * @author chenjianjx
 * 
 */
public class WebMethodStubSetFactory {

	public static WebMethodStubSet createWebMethodStubSet(Method method) {
		WebMethodStubSet stubSet = new WebMethodStubSet();
		stubSet.setMethodName(method.getName());
		
		WSDoc annotation=method.getAnnotation(WSDoc.class);
		if(annotation!=null)
			stubSet.setDescription(annotation.description());
		
		addRequestStubs(method, stubSet);
		if (!Void.TYPE.equals(method.getReturnType())) {
			addResponseStub(method, stubSet);
		}
		return stubSet;
	}

	private static void addResponseStub(Method method, WebMethodStubSet stubSet) {
		JavaLanguageVariable variable = createVariableFromMethodReturn(method);
		Stub stub = Variable2Stub.convertToStub(variable, stubSet.getStubTypeTreeRepository());
		stubSet.setResponseStub(stub);
	}

	private static void addRequestStubs(Method method, WebMethodStubSet stubSet) {
		List<JavaLanguageVariable> requestVariables = createVariablesFromMethodParamaters(method);
		for (JavaLanguageVariable variable : requestVariables) {
			Stub stub = Variable2Stub.convertToStub(variable, stubSet.getStubTypeTreeRepository());
			stubSet.addRequetStub(stub);
		}
	}

}
