package com.logicaldoc.webservice.doc.view.freemarker;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.logicaldoc.webservice.doc.JavaNameDisplayStrategy;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.doc.model.WebServiceStubSet;

import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateMethodModel;
import freemarker.template.TemplateModelException;

public class FreemarkerWebServiceDisplayer {

	private final Template template;

	private final WebServiceStubSet serviceStubSet;

	private final JavaNameDisplayStrategy nameDisplayingStrategy;

	FreemarkerWebServiceDisplayer(Template template, JavaNameDisplayStrategy nameDisplayingStrategy,
			WebServiceStubSet serviceStubSet) {
		this.template = template;
		this.nameDisplayingStrategy = nameDisplayingStrategy;
		this.serviceStubSet = serviceStubSet;
	}

	public String displayWebService() {

		try {
			Map<String, Object> rootMap = new HashMap<>();

			rootMap.put("service", serviceStubSet);

			rootMap.put("elementType", new DisplayElementTypeMethodModel());
			rootMap.put("elementName", new DisplayElementNameMethodModel());
			rootMap.put("className", new DisplayReadableClassNameMethodModel());
			rootMap.put("classDescription", new DisplayClassDescriptionMethodModel());

			StringWriter out = new StringWriter();
			template.process(rootMap, out);
			return out.toString();

		} catch (IOException e) {
			throw new IllegalStateException(e);
		} catch (TemplateException e) {
			throw new IllegalStateException(e);
		}

	}

	private Class<?> toClass(String className) {
		if (className.equals("byte"))
			return byte.class;
		if (className.equals("short"))
			return short.class;
		if (className.equals("int"))
			return int.class;
		if (className.equals("long"))
			return long.class;
		if (className.equals("char"))
			return char.class;
		if (className.equals("float"))
			return float.class;
		if (className.equals("double"))
			return double.class;
		if (className.equals("boolean"))
			return boolean.class;
		if (className.equals("void"))
			return void.class;

		if (className.startsWith("[L"))
			className = className.substring(2, className.length() - 1);
		else if (className.equals("[J"))
			className = Long.class.getName();
		else if (className.equals("[D"))
			className = Double.class.getName();

		try {
			return serviceStubSet.getWebServiceClass().getClassLoader().loadClass(className);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			System.out.println("Cannot generate html for type " + className + ". Will use 'UnkownType' instead");
			return UnkownType.class;
		}
	}

	private final class DisplayElementTypeMethodModel implements TemplateMethodModel {
		public Object exec(List arguments) throws TemplateModelException {
			String className = (String) arguments.get(0);
			Class<?> clazz = toClass(className);
			return nameDisplayingStrategy.displayElementType(clazz);
		}
	}

	private final class DisplayClassDescriptionMethodModel implements TemplateMethodModel {
		public Object exec(List arguments) throws TemplateModelException {
			String className = (String) arguments.get(0);
			try {
				Class<?> clazz = this.getClass().getClassLoader().loadClass(className);
				WSDoc annotation = clazz.getAnnotation(WSDoc.class);
				if (annotation != null) {
					return annotation.description();
				} else
					return "";
			} catch (Throwable t) {
				return "";
			}
		}
	}

	private final class DisplayElementNameMethodModel implements TemplateMethodModel {
		@SuppressWarnings("rawtypes")
		@Override
		public Object exec(List arguments) throws TemplateModelException {
			return nameDisplayingStrategy.displayElementName((String) arguments.get(0));
		}
	}

	private final class DisplayReadableClassNameMethodModel implements TemplateMethodModel {
		@SuppressWarnings("rawtypes")
		@Override
		public Object exec(List arguments) throws TemplateModelException {
			String className = (String) arguments.get(0);
			Class<?> clazz = toClass(className);
			return nameDisplayingStrategy.displayClassName(clazz);
		}
	}

	private static final class UnkownType {
		// Nothing to do
	}
}