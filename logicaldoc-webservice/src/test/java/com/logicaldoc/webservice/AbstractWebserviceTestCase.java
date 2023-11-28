package com.logicaldoc.webservice;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.web.context.support.XmlWebApplicationContext;

import com.logicaldoc.util.junit.AbstractTestCase;

/**
 * Abstract test case for the Web Service module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public abstract class AbstractWebserviceTestCase extends AbstractTestCase {

	@Override
	protected ApplicationContext buildApplicationContext() {
		WebserviceApplicationContext appContext = new WebserviceApplicationContext();
		appContext.refresh();
		return appContext;
	}
	
	
	
	@Override
	protected String[] getSqlScripts() {
		return new String[] { "/sql/logicaldoc-core.sql", "/sql/logicaldoc-webservice.sql", "/data.sql" };
	}

}