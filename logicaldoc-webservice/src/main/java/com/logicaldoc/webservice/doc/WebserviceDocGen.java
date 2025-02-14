package com.logicaldoc.webservice.doc;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.webservice.doc.model.WebMethodStubSet;
import com.logicaldoc.webservice.doc.model.WebServiceStubSet;
import com.logicaldoc.webservice.doc.service.WebServiceStubSetFactory;
import com.logicaldoc.webservice.doc.view.freemarker.ClasspathFreemarkerWebServiceDisplayEngine;
import com.logicaldoc.webservice.doc.view.freemarker.FreemarkerWebServiceDisplayEngine;
import com.logicaldoc.webservice.doc.view.simple.SimpleJavaNameDisplayStrategy;

public class WebserviceDocGen {

	private static final Logger console = LoggerFactory.getLogger("console");

	public static void main(String[] args) throws ClassNotFoundException {
		String release = args[0];
		File outputDir = new File(args[1]);

		List<Class<?>> services = new ArrayList<>();
		for (int i = 2; i < args.length; i++) {
			try {
				String name = args[i];
				if (!name.startsWith("com.logicaldoc."))
					name = "com.logicaldoc." + args[i];
				services.add(WebserviceDocGen.class.forName(name));
			} catch (Exception t) {
				console.error(t.getMessage());
			}
		}

		WebserviceDocGen doc = new WebserviceDocGen();
		doc.indexPage(release, outputDir, services);

		for (Class<?> clazz : services) {
			doc.singleReport(outputDir, clazz);
		}
	}

	public void indexPage(String release, File outputDir, List<Class<?>> services) {
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
		FileUtil.writeFile(html, outputFile.getAbsolutePath());

		console.info("Please find the HTML files at {}", outputFile.getAbsolutePath());
	}

	public void singleReport(File outputDir, Class<?> webServiceClass) {
		WebServiceStubSet serviceStubSet = WebServiceStubSetFactory.createWebServiceStubSet(webServiceClass);

		outputDir.mkdirs();

		FreemarkerWebServiceDisplayEngine displayEngine = ClasspathFreemarkerWebServiceDisplayEngine
				.createEngine(new SimpleJavaNameDisplayStrategy(), "/wsdoc.ftl");

		String html = displayEngine.displayWebService(serviceStubSet);
		String fileName = webServiceClass.getSimpleName();
		fileName += ".html";
		fileName = fileName.replace("Service.", ".");
		File outputFile = new File(outputDir, fileName);
		FileUtil.writeFile(html, outputFile.getAbsolutePath());

		console.info("Please find the HTML files at {}", outputFile.getAbsolutePath());
	}
}