package com.logicaldoc.cmis;

import com.logicaldoc.util.junit.AbstractTestCase;

/**
 * Abstract test case for the Webapp module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public abstract class AbstractCmisTestCase extends AbstractTestCase {

	static {
		System.setProperty("LOGICALDOC_REPOSITORY", "target");
	}

	@Override
	protected String[] getContexts() {
		return new String[] { "/contexttest.xml" };
	}

	@Override
	protected String[] getSqlScripts() {
		return new String[] { "/sql/logicaldoc-core.sql", "/data.sql" };
	}
}