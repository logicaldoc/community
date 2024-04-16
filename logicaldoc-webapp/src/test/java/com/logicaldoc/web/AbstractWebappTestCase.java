package com.logicaldoc.web;

import java.io.File;
import java.io.FileNotFoundException;
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
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.service.SecurityServiceImpl;
import com.logicaldoc.web.util.MockServletSession;

import junit.framework.Assert;

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
public abstract class AbstractWebappTestCase extends AbstractTestCase {

	protected File repositoryDir = new File(tempDir, "repository");

	protected GUISession guiSession;

	protected Session session;

	protected MockServletSession servletSession = new MockServletSession();

	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();

		repositoryDir.mkdirs();
		repositoryDir.mkdir();

		File docs2 = new File(repositoryDir, "docs2");
		docs2.mkdir();

		try {
			prepareSession("admin", "admin");
		} catch (ServerException e) {
			throw new IOException(e);
		}

		Assert.assertNotNull(guiSession);
		Assert.assertNotNull(SessionManager.get().get(guiSession.getSid()));
	}

	protected void prepareSession(String username, String password) throws ServerException, PersistenceException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		guiSession = new GUISession();
		Client client=new Client("xyz", "192.168.2.231", "ghost");
		Device device=new Device();
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
			guiSession = new SecurityServiceImpl().loadSession(session, null);

			LDSecurityContextRepository.bindServletSession(guiSession.getSid(), servletSession);
		}
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