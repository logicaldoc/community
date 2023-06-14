package com.logicaldoc.webservice;

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
	protected String[] getSqlScripts() {
		return new String[] { "/sql/logicaldoc-core.sql", "/sql/logicaldoc-webservice.sql", "/data.sql" };
	}

}