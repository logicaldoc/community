package com.logicaldoc.dropbox;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.sql.Connection;
import java.sql.ResultSet;

import javax.sql.DataSource;

import org.apache.commons.io.FileUtils;
import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlTool.SqlToolException;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Abstract test case for the Calendar module. This class initialises a test
 * database and prepares the spring test context.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public abstract class AbstractDropBoxTCase {

	protected ApplicationContext context;

	protected DataSource ds;

	protected File tempDir = new File("target/tmp");

	protected File coreSchemaFile;

	protected File calendarSchemaFile;

	protected File dataFile;

	private String userHome;

	@Before
	public void setUp() throws Exception {
		userHome = System.getProperty("user.home");
		System.setProperty("user.home", tempDir.getPath());

		try {
			context = new ClassPathXmlApplicationContext(new String[] { "/context.xml" });
		} catch (Throwable e) {
			e.printStackTrace();
		}

		createTestDirs();
		createTestDatabase();
	}

	protected void createTestDirs() throws IOException {
		// Create test dirs
		try {
			if (tempDir.exists() && tempDir.isDirectory())
				FileUtils.deleteDirectory(tempDir);
		} catch (Exception e) {
		}
		tempDir.mkdirs();

		Assert.assertTrue(tempDir.exists() && tempDir.isDirectory());

		coreSchemaFile = new File(tempDir, "logicaldoc-core.sql");
		calendarSchemaFile = new File(tempDir, "logicaldoc-calendar.sql");
		dataFile = new File(tempDir, "data.sql");

		// Copy sql files
		copyResource("/sql/logicaldoc-core.sql", coreSchemaFile.getCanonicalPath());
		copyResource("/sql/logicaldoc-calendar.sql", calendarSchemaFile.getCanonicalPath());
		copyResource("/data.sql", dataFile.getCanonicalPath());
	}

	protected void copyResource(String classpath, String destinationPath) throws IOException {
		copyResource(classpath, new File(destinationPath));
	}

	/**
	 * Copy a resource from the classpath into a file
	 * 
	 * @param classpath The classpath specification
	 * @param out The target file
	 * @throws IOException
	 */
	protected void copyResource(String classpath, File out) throws IOException {
		InputStream is = new BufferedInputStream(this.getClass().getResource(classpath).openStream());
		OutputStream os = new BufferedOutputStream(new FileOutputStream(out));
		try {
			for (;;) {
				int b = is.read();
				if (b == -1)
					break;
				os.write(b);
			}
		} finally {
			is.close();
			os.close();
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
		try {
			con = ds.getConnection();
			con.createStatement().execute("shutdown");
		} catch (Exception e) {
			try {
				if (con != null)
					con.close();
			} catch (Exception ex) {
			}
			e.printStackTrace();
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
			SqlFile sqlFile = new SqlFile(coreSchemaFile, "Cp1252", false);
			sqlFile.setConnection(con);
			sqlFile.execute();
			sqlFile = new SqlFile(calendarSchemaFile, "Cp1252", false);
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
