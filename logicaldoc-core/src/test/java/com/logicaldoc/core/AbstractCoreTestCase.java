package com.logicaldoc.core;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.springframework.security.core.context.SecurityContextHolder;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionDAO;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDDeferredSecurityContext;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletSession;
import com.logicaldoc.util.spring.Context;

/**
 * Abstract test case for the Core module. This class initializes a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public abstract class AbstractCoreTestCase extends AbstractTestCase {

	protected File rootStoreOne;

	protected File rootStoreTwo;

	protected ApiKey apiKey;

	protected MockServletSession servletSession = new MockServletSession();

	protected Session session;


	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		prepareStore();

		// Prepare an API Key
		ApiKeyDAO dao = Context.get(ApiKeyDAO.class);
		apiKey = new ApiKey(1L, "MyKey");
		dao.store(apiKey);
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("sql/logicaldoc-core.sql", "data.sql");
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
		FileUtil.copyResource("loremipsum.pdf", new File(rootStoreOne.getPath() + "/1/doc/1.0"));
		FileUtil.copyResource("loremipsum.pdf", new File(rootStoreOne.getPath() + "/1/doc/1.0-conversion.pdf"));

		// Store the file of document 3
		FileUtil.copyResource("small.pdf", new File(rootStoreOne.getPath() + "/3/doc/1.3"));

		// Store the file of document 8
		FileUtil.copyResource("small.pdf", new File(rootStoreOne.getPath() + "/8/doc/1.0"));
	}

	@Override
	public void tearDown() throws IOException, SQLException {
		if (session != null)
			Context.get(SessionManager.class).kill(session.getSid());
		Context.get(SessionDAO.class).cleanOldSessions(-1);

		super.tearDown();

		FileUtil.delete(rootStoreOne);
		FileUtil.delete(rootStoreTwo);
	}

	protected void prepareSession(String username, String password) throws PersistenceException {
		UserDAO userDao = Context.get(UserDAO.class);

		Client client = new Client("xyz", "192.168.2.231", "ghost");
		Device device = new Device();
		device.setBrowser("Firefox");
		device.setBrowserVersion("18");
		device.setOperativeSystem("Windows");
		client.setDevice(device);
		session = SessionManager.get().newSession(username, password, null, client);
		if (session != null) {
			User user = userDao.findByUsernameIgnoreCase(username);
			userDao.initialize(user);
			LDAuthenticationToken token = new LDAuthenticationToken(username);
			token.setSid(session.getSid());
			SecurityContextHolder.getContext().setAuthentication(token);
			LDDeferredSecurityContext.bindServletSession(session.getSid(), servletSession);
		}
	}
}