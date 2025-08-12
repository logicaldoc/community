package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.junit.AbstractWebappTestCase;

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
public abstract class AbstractWPTestCase extends AbstractWebappTestCase {

	protected File repositoryDir = new File(tempDir, "repository");

	@Override
	protected ApplicationContext buildApplicationContext() {
		return new AnnotationConfigApplicationContext(WebappTestContext.class);
	}

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		repositoryDir.mkdirs();
		repositoryDir.mkdir();

		File docs = new File(repositoryDir, "docs");
		docs.mkdir();
		File docDir = new File(docs + "/1/doc");
		docDir.mkdirs();
		docDir.mkdir();
		FileUtil.copyResource("pdf1.pdf", new File(docDir, "1.0"));
		docDir = new File(docs + "/3/doc");
		docDir.mkdirs();
		docDir.mkdir();
		FileUtil.copyResource("pdf2.pdf", new File(docDir, "1.1"));

		File docs2 = new File(repositoryDir, "docs2");
		docs2.mkdir();
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("sql/logicaldoc-core.sql", "data.sql");
	}
}