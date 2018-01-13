package com.logicaldoc.web;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;

import javax.sql.DataSource;

import junit.framework.Assert;

import org.apache.commons.io.FileUtils;
import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlTool.SqlToolException;
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
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.service.SecurityServiceImpl;

/**
 * Abstract test case for the Webapp module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public abstract class AbstractWebappTCase {

	protected ApplicationContext context;

	protected DataSource ds;

	protected File tempDir = new File("target/tmp");

	protected File dbSchemaFile;

	protected File dataFile;

	private String userHome;

	protected GUISession session;

	static {
		System.setProperty("LOGICALDOC_REPOSITORY", "target");
	}

	@Before
	public void setUp() throws Exception {
		tempDir = new File("target/tmp");

		userHome = System.getProperty("user.home");
		System.setProperty("user.home", tempDir.getPath());
		context = new ClassPathXmlApplicationContext(new String[] { "/contexttest.xml" });
		createTestDirs();
		createTestDatabase();

		this.session = prepareSession();
		Assert.assertNotNull(session);
		Assert.assertNotNull(SessionManager.get().get(session.getSid()));

	}

	private GUISession prepareSession() {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		GUISession session = new GUISession();
		Session sess = SessionManager.get().newSession("admin", "admin", null, new Client());
		if (sess != null) {
			User user = userDao.findByUsernameIgnoreCase("admin");
			userDao.initialize(user);
			LDAuthenticationToken token = new LDAuthenticationToken("admin");
			token.setSid(sess.getSid());
			SecurityContextHolder.getContext().setAuthentication(token);
			session = new SecurityServiceImpl().loadSession(sess, null);
		}

		return session;
	}

	protected void createTestDirs() throws IOException {
		// Create test dirs
		try {
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
		} catch (Exception e) {
		}
		tempDir.mkdirs();

		dbSchemaFile = new File(tempDir, "logicaldoc-core.sql");
		dataFile = new File(tempDir, "data.sql");

		// Copy sql files
		copyResource("/sql/logicaldoc-core.sql", dbSchemaFile.getCanonicalPath());
		copyResource("/data.sql", dataFile.getCanonicalPath());
	}

	protected void copyResource(String classpath, String destinationPath) throws IOException {
		FileUtil.copyResource(classpath, new File(destinationPath));
	}

	@After
	public void tearDown() throws Exception {
		destroyDatabase();
		((AbstractApplicationContext) context).close();

		// Restore user home system property
		System.setProperty("user.home", userHome);
	}

	/**
	 * Destroys the in-memory database
	 */
	private void destroyDatabase() {
		Assert.assertNotNull(ds);
		Connection con = null;
		try {
			con = ds.getConnection();
			con.createStatement().execute("SHUTDOWN IMMEDIATELY");
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (con != null)
					con.close();
			} catch (Exception ex) {
				ex.printStackTrace();
			}
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

		Connection con = null;
		try {
			con = ds.getConnection();

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
		} finally {
			if (con != null)
				con.close();
		}
	}
}