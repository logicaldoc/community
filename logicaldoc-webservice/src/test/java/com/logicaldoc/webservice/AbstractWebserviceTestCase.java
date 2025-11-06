package com.logicaldoc.webservice;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
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

	protected Session session;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		ApiKeyDAO dao = ApiKeyDAO.get();
		apiKey = new ApiKey(1L, "MyKey");
		dao.store(apiKey);

		prepareSession();
	}

	protected void prepareSession() {
		Client client = new Client("xyz", "192.168.2.231", "ghost");
		Device device = new Device();
		device.setBrowser("Firefox");
		device.setBrowserVersion("18");
		device.setOperativeSystem("Windows");
		client.setDevice(device);
		session = SessionManager.get().newSession("admin", "admin", null, client);
	}

	@Override
	public void tearDown() throws IOException {
		SessionManager.get().kill(session.getSid());
		super.tearDown();
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("sql/logicaldoc-core.sql", "sql/logicaldoc-webservice.sql", "data.sql");
	}
}