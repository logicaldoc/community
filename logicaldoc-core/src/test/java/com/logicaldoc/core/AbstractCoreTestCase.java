package com.logicaldoc.core;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;

import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.junit.AbstractTestCase;

/**
 * Abstract test case for the Core module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public abstract class AbstractCoreTestCase extends AbstractTestCase {

	@Before
	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		prepareStore();
	}

	@Override
	protected String[] getSqlScripts() {
		return new String[] { "/sql/logicaldoc-core.sql", "/data.sql" };
	}

	private void prepareStore() throws IOException {
		String storePath = Context.get().getProperties().getProperty("store.1.dir");
		new File(storePath).mkdir();
		new File(Context.get().getProperties().getProperty("store.2.dir")).mkdir();

		Storer storer = (Storer) context.getBean("Storer");
		storer.init();

		// Store the file of document 1
		FileUtil.copyResource("/Digital_Day.pdf", new File(storePath + "/1/doc/1.0"));
		FileUtil.copyResource("/Digital_Day.pdf", new File(storePath + "/1/doc/1.0-conversion.pdf"));

		// Store the file of document 3
		FileUtil.copyResource("/small.pdf", new File(storePath + "/3/doc/1.3"));
	}
}