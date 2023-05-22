package com.logicaldoc.webservice.doc.view.freemarker;

import com.logicaldoc.webservice.doc.JavaNameDisplayStrategy;
import com.logicaldoc.webservice.doc.WebServiceDisplayEngine;
import com.logicaldoc.webservice.doc.model.WebServiceStubSet;

import freemarker.template.Configuration;
import freemarker.template.DefaultObjectWrapper;
import freemarker.template.Template;

public abstract class FreemarkerWebServiceDisplayEngine extends WebServiceDisplayEngine {

	protected Configuration configuration;

	protected FreemarkerWebServiceDisplayEngine(JavaNameDisplayStrategy nameDisplayingStrategy) {
		super(nameDisplayingStrategy);
		configuration = new Configuration();
		configuration.setLocalizedLookup(false);
		configuration.setObjectWrapper(new DefaultObjectWrapper());
	}

	@Override
	public String displayWebService(WebServiceStubSet serviceStubSet) {
		Template template = getTemplate();
		FreemarkerWebServiceDisplayer displayer = new FreemarkerWebServiceDisplayer(template, nameDisplayingStrategy,
				serviceStubSet);
		return displayer.displayWebService();
	}

	protected abstract Template getTemplate();

}
