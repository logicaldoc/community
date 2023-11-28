package com.logicaldoc.webservice;

import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.web.context.support.XmlWebApplicationContext;

/**
 * This context acts like a ClassPathXmlApplicationContext but extends the
 * XmlWebApplicationContext in order have some scopes like 'request' enabled.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class WebserviceApplicationContext extends XmlWebApplicationContext {

	@Override
	protected String[] getDefaultConfigLocations() {
		return new String[] { "/context.xml" };
	}

	@Override
	protected Resource getResourceByPath(String path) {
		return new ClassPathResource(path);
	}
}
