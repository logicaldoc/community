package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.sql.DataSource;

import org.apache.commons.io.FileUtils;
import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlTool.SqlToolException;
import org.java.plugin.JpfException;
import org.junit.After;
import org.junit.Before;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.security.core.context.SecurityContextHolder;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDSecurityContextRepository;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.web.service.SecurityServiceImpl;

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
public abstract class AbstractWebappTCase {

	protected ApplicationContext context;

	protected DataSource ds;

	protected File repositoryDir = new File("target/repository");

	protected File dbSchemaFile;

	protected File dataFile;

	protected GUISession guiSession;

	protected Session session;

	protected MockServletSession servletSession = new MockServletSession();

	@Before
	public void setUp() throws Exception {
		System.setProperty("solr.http1", "true");

		context = new ClassPathXmlApplicationContext(new String[] { "/contexttest.xml" });
		createTestDirs();
		createTestDatabase();

		prepareSession("admin", "admin");
		Assert.assertNotNull(guiSession);
		Assert.assertNotNull(SessionManager.get().get(guiSession.getSid()));
	}

	@After
	public void tearDown() throws Exception {
		destroyDatabase();
		((AbstractApplicationContext) context).close();
	}
	
	protected void prepareSession(String username, String password) throws ServerException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		guiSession = new GUISession();
		session = SessionManager.get().newSession(username, password, null, new Client());
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

	protected void createTestDirs() throws IOException {
		deleteTestDirs();
		repositoryDir.mkdirs();
		repositoryDir.mkdir();

		dbSchemaFile = new File(repositoryDir, "logicaldoc-core.sql");
		dataFile = new File(repositoryDir, "data.sql");

		// Copy sql files
		copyResource("/sql/logicaldoc-core.sql", dbSchemaFile.getCanonicalPath());
		copyResource("/data.sql", dataFile.getCanonicalPath());
	}

	private void deleteTestDirs() {
		// Create test dirs
		try {
			if (repositoryDir.exists() && repositoryDir.isDirectory())
				FileUtils.deleteDirectory(repositoryDir);
		} catch (Exception e) {
			// Nothing to do
		}
	}

	protected void copyResource(String classpath, String destinationPath) throws IOException {
		FileUtil.copyResource(classpath, new File(destinationPath));
	}

	/**
	 * Destroys the in-memory database
	 * 
	 * @throws SQLException error at database level
	 */
	private void destroyDatabase() throws SQLException {
		try (Connection con = ds.getConnection();) {
			con.createStatement().execute("shutdown");
		}
	}

	/**
	 * Creates an in-memory test database
	 * 
	 * @throws SqlToolException
	 * 
	 */
	private void createTestDatabase() throws Exception {
		ds = (DataSource) context.getBean("DataSource");
		Assert.assertNotNull(ds);

		try (Connection con = ds.getConnection();) {
			// Load schema
			SqlFile sqlFile = new SqlFile(dbSchemaFile, "Cp1252", false);
			sqlFile.setConnection(con);
			sqlFile.execute();

			// Load data
			sqlFile = new SqlFile(dataFile, "Cp1252", false);
			sqlFile.setConnection(con);
			sqlFile.execute();

			// Test the connection
			ResultSet rs = con.createStatement().executeQuery("select * from ld_menu where ld_id=2");
			rs.next();

			Assert.assertEquals(2, rs.getInt(1));
		}
	}
}