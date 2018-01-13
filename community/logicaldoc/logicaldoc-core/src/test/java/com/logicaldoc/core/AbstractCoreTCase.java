package com.logicaldoc.core;

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

import com.logicaldoc.util.io.FileUtil;

/**
 * Abstract test case for the Core module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.0
 */
public abstract class AbstractCoreTCase {

	protected ApplicationContext context;

	protected DataSource ds;

	protected File tempDir = new File("target/tmp");

	protected File dbSchemaFile;

	protected File dataFile;

	private String userHome;

	static {
		System.setProperty("LOGICALDOC_REPOSITORY", "target");
	}

	@Before
	public void setUp() throws Exception {
		tempDir = new File("target/tmp");
		userHome = System.getProperty("user.home");
		System.setProperty("user.home", tempDir.getPath());
		
		createTestDirs();
		context = new ClassPathXmlApplicationContext(new String[] { "/context.xml" });
		createTestDatabase();
	}

	protected void createTestDirs() throws IOException {
		// Create test dirs
		try {
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		if (tempDir.exists())
			throw new IOException("unable to delete folder " + tempDir.getPath());

		tempDir.mkdirs();

		dbSchemaFile = new File(tempDir, "logicaldoc-core.sql");
		dataFile = new File(tempDir, "data.sql");

		// Copy sql files
		copyResource("/sql/logicaldoc-core.sql", dbSchemaFile.getCanonicalPath());
		copyResource("/data.sql", dataFile.getCanonicalPath());
	}

	protected void copyResource(String classpath, String destinationPath) throws IOException {
		try {
			FileUtil.copyResource(classpath, new File(destinationPath));
		} catch (Exception e) {
			e.printStackTrace();
		}
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
		Connection con = null;
		if (ds == null)
			return;
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