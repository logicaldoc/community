package com.logicaldoc.core;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.sql.DataSource;

import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlTool.SqlToolException;
import org.junit.After;
import org.junit.Before;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

import junit.framework.Assert;

/**
 * Abstract test case for the Core module. This class initialises a test
 * database and prepares the spring test context.
 * <p>
 * All LogicalDOC's tests must extend this test case in order to find a ready
 * and accessible database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public abstract class AbstractCoreTCase {

	protected ApplicationContext context;

	protected DataSource ds;

	protected File tempDir = new File("target/tmp");

	protected File dbSchemaFile = new File("src/main/resources/sql/logicaldoc-core.sql");

	protected File dataFile = new File("src/test/resources/data.sql");

	private String userHome;

	@Before
	public void setUp() throws Exception {
		userHome = System.getProperty("user.home");
		System.setProperty("user.home", tempDir.getPath());
		System.setProperty("solr.http1", "true");

		createTestDirs();
		context = new ClassPathXmlApplicationContext(new String[] { "/context.xml" });
		createTestDatabase();
		
		prepareStore();
	}

	private void prepareStore() throws IOException {
		String storePath = Context.get().getProperties().getPropertyWithSubstitutions("store.1.dir");
		new File(storePath).mkdir();
		new File(Context.get().getProperties().getPropertyWithSubstitutions("store.2.dir")).mkdir();
		
		Storer storer = (Storer) context.getBean("Storer");
		storer.init();
				
		// Store the file of document 1
		FileUtil.copyResource("/Digital_Day.pdf", new File(storePath+"/1/doc/1.0"));
		
		// Store the file of document 3
		FileUtil.copyResource("/small.pdf", new File(storePath+"/3/doc/1.3"));
	}

	protected void createTestDirs() throws IOException {
		FileUtil.strongDelete(tempDir);
		FileUtil.strongDelete(tempDir);
		FileUtil.strongDelete(tempDir);
		tempDir.mkdirs();
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
	 * 
	 * @throws SQLException error at database level
	 */
	private void destroyDatabase() throws SQLException {
		try (Connection con = ds.getConnection()) {
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
		try (Connection con = ds.getConnection()) {

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