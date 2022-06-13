package com.logicaldoc.webservice.doc.view.freemarker;

import java.io.IOException;

import com.logicaldoc.webservice.doc.JavaNameDisplayStrategy;
import com.logicaldoc.webservice.doc.view.simple.SimpleJavaNameDisplayStrategy;

import freemarker.template.Template;

public class ClasspathFreemarkerWebServiceDisplayEngine extends FreemarkerWebServiceDisplayEngine {

	private String absoluteFtlClassPath;

	private ClasspathFreemarkerWebServiceDisplayEngine(JavaNameDisplayStrategy nameDisplayingStrategy,
			String absoluteFtlClassPath) {
		super(nameDisplayingStrategy);

		if (!absoluteFtlClassPath.startsWith("/")) {
			throw new IllegalArgumentException("Template's class-path has to start with '/'");
		}
		configuration.setClassForTemplateLoading(ClasspathFreemarkerWebServiceDisplayEngine.class, "/");
		this.absoluteFtlClassPath = absoluteFtlClassPath;
	}

	public static FreemarkerWebServiceDisplayEngine createEngine(JavaNameDisplayStrategy nameDisplayingStrategy,
			String absoluteFtlClassPath) {
		return new ClasspathFreemarkerWebServiceDisplayEngine(nameDisplayingStrategy, absoluteFtlClassPath);
	}

	public static FreemarkerWebServiceDisplayEngine createEngine(SimpleJavaNameDisplayStrategy nameDisplayingStrategy) {
		String ftlPath = "/wsdoc.ftl";
		return createEngine(nameDisplayingStrategy, ftlPath);
	}

	protected Template getTemplate() {
		try {
			return configuration.getTemplate(absoluteFtlClassPath);
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

}
