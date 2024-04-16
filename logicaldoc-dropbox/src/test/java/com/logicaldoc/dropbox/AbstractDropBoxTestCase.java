package com.logicaldoc.dropbox;

import java.util.List;

import com.logicaldoc.util.junit.AbstractTestCase;

/**
 * Abstract test case for the Calendar module. This class initialises a test
 * database and prepares the spring test context.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public abstract class AbstractDropBoxTestCase extends AbstractTestCase {

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("/sql/logicaldoc-core.sql", "/sql/logicaldoc-dropbox.sql", "/data.sql");
	}
}
