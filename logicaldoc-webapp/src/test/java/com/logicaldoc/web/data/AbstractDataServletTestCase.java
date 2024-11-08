package com.logicaldoc.web.data;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.After;
import org.junit.Before;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWebappTestCase;

public abstract class AbstractDataServletTestCase extends AbstractWebappTestCase {

	protected final File responseFile = new File("target/response.xml");

	public AbstractDataServletTestCase() {
		super();
	}

	@Override
	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		clearResponseFile();
	}

	@Override
	@After
	public void tearDown() throws SQLException {
		super.tearDown();
		clearResponseFile();
	}
	
	protected void clearResponseFile() {
		FileUtil.delete(responseFile);
	}
}