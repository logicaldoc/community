package com.logicaldoc.webservice.doc.service;

import static com.logicaldoc.webservice.doc.service.WebMethodStubSetFactory.createWebMethodStubSet;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.jws.WebMethod;

import com.logicaldoc.webservice.doc.model.WebServiceStubSet;

public class WebServiceStubSetFactory {

	private WebServiceStubSetFactory() {
	}

	public static WebServiceStubSet createWebServiceStubSet(Class<?> webServiceClass) {
		List<Method> methods = getWebMethods(webServiceClass);
		WebServiceStubSet serviceStubs = new WebServiceStubSet();
		serviceStubs.setWebServiceClass(webServiceClass);
		for (Method method : methods) {
			serviceStubs.addMethodStub(createWebMethodStubSet(method));
		}
		return serviceStubs;
	}

	private static List<Method> getWebMethods(Class<?> webServiceClass) {
		List<Method> webMethods = new ArrayList<>();
		for (Method method : webServiceClass.getMethods()) {
			if (method.isAnnotationPresent(WebMethod.class)) {
				webMethods.add(method);
			}
		}
		return webMethods;
	}
}
