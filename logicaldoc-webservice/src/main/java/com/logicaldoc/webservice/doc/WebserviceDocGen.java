package com.logicaldoc.webservice.doc;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.webservice.doc.model.WebMethodStubSet;
import com.logicaldoc.webservice.doc.model.WebServiceStubSet;
import com.logicaldoc.webservice.doc.service.WebServiceStubSetFactory;
import com.logicaldoc.webservice.doc.view.freemarker.ClasspathFreemarkerWebServiceDisplayEngine;
import com.logicaldoc.webservice.doc.view.freemarker.FreemarkerWebServiceDisplayEngine;
import com.logicaldoc.webservice.doc.view.simple.SimpleJavaNameDisplayStrategy;

public class WebserviceDocGen {

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		String release = args[0];
		File outputDir = new File(args[1]);

		List<Class<?>> services = new ArrayList<>();
		for (int i = 2; i < args.length; i++) {
			try {
				String name = args[i];
				if (!name.startsWith("com.logicaldoc."))
					name = "com.logicaldoc." + args[i];
				services.add(WebserviceDocGen.class.forName(name));
			} catch (Throwable t) {
				System.err.println(t.getMessage());
			}
		}

		WebserviceDocGen doc = new WebserviceDocGen();
		doc.indexPage(release, outputDir, services);

		for (Class<?> clazz : services) {
			doc.singleReport(outputDir, clazz);
		}
	}

	public void indexPage(String release, File outputDir, List<Class<?>> services) throws IOException {
		outputDir.mkdirs();

		FreemarkerWebServiceDisplayEngine displayEngine = ClasspathFreemarkerWebServiceDisplayEngine
				.createEngine(new SimpleJavaNameDisplayStrategy(), "/wsdocindex.ftl");

		WebServiceStubSet serviceStubSet = new WebServiceStubSet();
		serviceStubSet.setRelease(release);
		for (Class<?> clazz : services) {
			WebMethodStubSet stub = new WebMethodStubSet();
			String name = clazz.getSimpleName() + ".";
			name = name.replace("Service.", "");
			stub.setMethodName(name);
			WSDoc annotation = clazz.getAnnotation(WSDoc.class);
			if (annotation != null)
				stub.setDescription(annotation.description());
			serviceStubSet.addMethodStub(stub);
		}

		String html = displayEngine.displayWebService(serviceStubSet);
		String fileName = "index.html";
		File outputFile = new File(outputDir, fileName);
		FileUtils.writeStringToFile(outputFile, html);

		System.out.println("Please find the HTML files at " + outputFile.getAbsolutePath());
	}

	public void singleReport(File outputDir, Class<?> webServiceClass) throws IOException {
		WebServiceStubSet serviceStubSet = WebServiceStubSetFactory.createWebServiceStubSet(webServiceClass);

		outputDir.mkdirs();

		FreemarkerWebServiceDisplayEngine displayEngine = ClasspathFreemarkerWebServiceDisplayEngine
				.createEngine(new SimpleJavaNameDisplayStrategy(), "/wsdoc.ftl");

		String html = displayEngine.displayWebService(serviceStubSet);
		String fileName = webServiceClass.getSimpleName();
		fileName += ".html";
		fileName = fileName.replace("Service.", ".");
		File outputFile = new File(outputDir, fileName);
		FileUtils.writeStringToFile(outputFile, html);

		System.out.println("Please find the HTML files at " + outputFile.getAbsolutePath());
	}
}