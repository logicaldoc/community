package com.logicaldoc.webservice;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.springframework.context.ApplicationContext;

import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Abstract test case for the Web Service module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public abstract class AbstractWebserviceTestCase extends AbstractTestCase {

	protected ApiKey apiKey;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		ApiKeyDAO dao = (ApiKeyDAO) Context.get().getBean(ApiKeyDAO.class);
		apiKey = new ApiKey(1L, "MyKey");
		dao.store(apiKey);
	}

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