package com.logicaldoc.core;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.core.store.Store;
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

	protected File rootStoreOne;

	protected File rootStoreTwo;

	protected ApiKey apiKey;

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		prepareStore();

		// Prepare an API Key
		ApiKeyDAO dao = (ApiKeyDAO) Context.get().getBean(ApiKeyDAO.class);
		apiKey = new ApiKey(1L, "MyKey");
		dao.store(apiKey);
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("/sql/logicaldoc-core.sql", "/data.sql");
	}

	private void prepareStore() throws IOException {
		/**
		 * For each test we must prepare different stores folders because
		 * re-using the same paths cause locks
		 */
		rootStoreOne = new File(Context.get().getProperties().getProperty("store.1.dir"));
		rootStoreTwo = new File(Context.get().getProperties().getProperty("store.2.dir"));

		Store store = (Store) context.getBean("Store");
		store.init();

		/*
		 * In order to minimize the locks, we write the file only if really
		 * needed
		 */

		// Store the file of document 1
		FileUtil.copyResource("/loremipsum.pdf", new File(rootStoreOne.getPath() + "/1/doc/1.0"));
		FileUtil.copyResource("/loremipsum.pdf", new File(rootStoreOne.getPath() + "/1/doc/1.0-conversion.pdf"));

		// Store the file of document 3
		FileUtil.copyResource("/small.pdf", new File(rootStoreOne.getPath() + "/3/doc/1.3"));
	}

	@Override
	public void tearDown() throws SQLException {
		super.tearDown();

		FileUtil.delete(rootStoreOne);
		FileUtil.delete(rootStoreTwo);
	}
}