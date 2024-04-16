package com.logicaldoc.webservice;

import java.util.List;

import org.springframework.context.ApplicationContext;

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
	protected List<String> getDatabaseScripts() {
		return List.of("/sql/logicaldoc-core.sql", "/sql/logicaldoc-webservice.sql", "/data.sql");
	}

}