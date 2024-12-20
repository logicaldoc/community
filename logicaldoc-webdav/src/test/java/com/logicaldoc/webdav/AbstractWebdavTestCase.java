package com.logicaldoc.webdav;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.security.core.context.SecurityContextHolder;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDSecurityContextRepository;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletSession;
import com.logicaldoc.webdav.session.DavSessionImpl;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * Abstract test case for the Webdav module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.3
 */
public abstract class AbstractWebdavTestCase extends AbstractTestCase {

	protected File repositoryDir = new File(tempDir, "repository");

	protected Session session;

	protected WebdavSession davSession;

	protected MockServletSession servletSession = new MockServletSession();

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		prepareRepository();
		prepareSession("admin", "admin");
		assertNotNull(session);
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

			LDSecurityContextRepository.bindServletSession(session.getSid(), servletSession);

			davSession = new DavSessionImpl();
			davSession.putObject("id", session.getUserId());
			davSession.putObject("sid", session.getSid());
		}
	}

	private void prepareRepository() throws IOException {
		repositoryDir.mkdirs();
		repositoryDir.mkdir();

		File file3 = new File(repositoryDir.getPath() + "/docs/1/doc/1.0");
		file3.getParentFile().mkdirs();
		FileUtil.copyResource("/pdf1.pdf", file3);

		File file5 = new File(repositoryDir.getPath() + "/docs/5/doc/1.0");
		file5.getParentFile().mkdirs();
		FileUtil.copyResource("/pdf2.pdf", file5);
		FileUtil.copyResource("/pdf2.pdf", new File(repositoryDir.getPath() + "/docs/5/doc/1.0-conversion.pdf"));
	}

	@Override
	protected ApplicationContext buildApplicationContext() {
		return new ClassPathXmlApplicationContext(new String[] { "/context.xml" });
	}

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("/sql/logicaldoc-core.sql", "/data.sql");
	}
}