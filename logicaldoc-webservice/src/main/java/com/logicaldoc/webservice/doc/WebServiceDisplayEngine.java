package com.logicaldoc.webservice.doc;

import com.logicaldoc.webservice.doc.model.WebServiceStubSet;

/**
 * Display stubs of a web method as readable String
 */
public abstract class WebServiceDisplayEngine {

	protected JavaNameDisplayStrategy nameDisplayingStrategy;

	public abstract String displayWebService(WebServiceStubSet serviceStubSet);

	protected WebServiceDisplayEngine(JavaNameDisplayStrategy nameDisplayingStrategy) {
		super();
		this.nameDisplayingStrategy = nameDisplayingStrategy;
	}

	public JavaNameDisplayStrategy getNameDisplayingStrategy() {
		return nameDisplayingStrategy;
	}

	public void setNameDisplayingStrategy(JavaNameDisplayStrategy nameDisplayingStyle) {
		this.nameDisplayingStrategy = nameDisplayingStyle;
	}
}