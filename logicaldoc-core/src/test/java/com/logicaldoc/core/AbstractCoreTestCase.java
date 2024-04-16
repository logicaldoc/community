package com.logicaldoc.core;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.plugin.PluginException;

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

	protected static Logger log = LoggerFactory.getLogger(AbstractCoreTestCase.class);

	@Before
	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();
		prepareStore();
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("/sql/logicaldoc-core.sql", "/data.sql");
	}

	private void prepareStore() throws IOException {
		String storePath = Context.get().getProperties().getProperty("store.1.dir");
		File store1 = new File(storePath);
		FileUtil.strongDelete(store1);
		store1.mkdir();

		File store2 = new File(Context.get().getProperties().getProperty("store.2.dir"));
		FileUtil.strongDelete(store2);
		store2.mkdir();

		Storer storer = (Storer) context.getBean("Storer");
		storer.init();

		// Store the file of document 1
		FileUtil.copyResource("/Digital_Day.pdf", new File(storePath + "/1/doc/1.0"));
		try {
			FileUtil.copyResource("/Digital_Day.pdf", new File(storePath + "/1/doc/1.0-conversion.pdf"));
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}

		// Store the file of document 3
		FileUtil.copyResource("/small.pdf", new File(storePath + "/3/doc/1.3"));
	}

	@Override
	public void tearDown() throws SQLException {
		super.tearDown();
	}
}