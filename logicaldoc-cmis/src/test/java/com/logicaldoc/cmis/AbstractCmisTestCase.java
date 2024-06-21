package com.logicaldoc.cmis;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;
import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.plugin.PluginException;

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
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		/*
		 * Prepare a test store file
		 */
		String storePath = Context.get().getProperties().getProperty("store.1.dir");
		File store = new File(storePath);

		FileUtil.delete(store);

		store.mkdir();
		new File(store, "5/doc").mkdirs();

		Storer storer = (Storer) context.getBean("Storer");
		storer.init();

		ResourceUtil.copyResource("/data.sql", new File(store, "5/doc/1.0"));
	}

	@Override
	protected ApplicationContext buildApplicationContext() {
		return new ClassPathXmlApplicationContext(new String[] { "/contexttest.xml" });
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("/sql/logicaldoc-core.sql", "/data.sql");
	}
}