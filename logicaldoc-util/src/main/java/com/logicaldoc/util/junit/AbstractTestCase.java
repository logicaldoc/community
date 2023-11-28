package com.logicaldoc.util.junit;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlToolError;
import org.junit.After;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.logicaldoc.util.io.FileUtil;

/**
 * Abstract test case that of database and context initialization.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.8.5
 */
public abstract class AbstractTestCase {

	private static Logger log = LoggerFactory.getLogger(AbstractTestCase.class);

	private static final String USER_HOME = "user.home";

	protected ApplicationContext context;

	protected File tempDir = new File("target/tmp");

	private String userHome;

	@Before
	public void setUp() throws IOException, SQLException {
		loadDevelSettings();

		updateUserHome();

		createTestDirs();

		context = buildApplicationContext();

		createTestDatabase();
	}

	@After
	public void tearDown() throws SQLException {
		try {
			destroyDatabase();

			if (context != null)
				((AbstractApplicationContext) context).close();
		} finally {
			restoreUserHome();
		}
	}

	private void updateUserHome() {
		userHome = System.getProperty(USER_HOME);
		System.setProperty(USER_HOME, tempDir.getPath());
	}

	/**
	 * Concrete implementations should  prepare and return the Spring ApplicationContext to use. 
	 * By default it is used the ClassPathXmlApplicationContext loading the /context.xml resource.
	 *  
	 * @return the ApplicationContext
	 */
	protected ApplicationContext buildApplicationContext() {
		return new ClassPathXmlApplicationContext(new String[] { "/context.xml" });
	}
	
	/**
	 * Concrete implementations should return the array of sql script resources
	 * to use to setup the database
	 * 
	 * @return array of resources(database script files)
	 */
	protected String[] getSqlScripts() {
		return new String[0];
	}

	/**
	 * Loads the settigs in the file user.home/logicaldoc-dev.properties putting
	 * them as Java variables
	 * 
	 * @throws IOException Error reading the development file
	 */
	private void loadDevelSettings() throws IOException {
		Properties devSettings = new Properties();
		try (FileReader reader = new FileReader(
				new File(System.getProperty(USER_HOME) + "/logicaldoc-dev.properties"))) {
			devSettings.load(reader);
			for (Map.Entry<Object, Object> entry : devSettings.entrySet())
				System.setProperty(entry.getKey().toString(), entry.getValue().toString());
		}
	}

	protected void createTestDirs() {
		FileUtil.strongDelete(tempDir);
		FileUtil.strongDelete(tempDir);
		FileUtil.strongDelete(tempDir);
		tempDir.mkdirs();
	}

	private void restoreUserHome() {
		// Restore user home system property
		System.setProperty(USER_HOME, userHome);
	}

	/**
	 * Destroys the in-memory database
	 * 
	 * @throws SQLException error at database level
	 */
	private void destroyDatabase() throws SQLException {
		if (getSqlScripts().length < 1)
			return;

		try (Connection con = getConnection(); Statement statement = con.createStatement()) {
			statement.execute("shutdown");
		}
	}

	protected Connection getConnection() throws SQLException {
		DataSource ds = (DataSource) context.getBean("DataSource");
		return ds.getConnection();
	}

	/**
	 * Creates an in-memory test database
	 * 
	 * @throws SQLException Error in one of the SQL scripts
	 * @throws IOException Error reading one of the SQL scripts
	 */
	private void createTestDatabase() throws SQLException, IOException {
		if (getSqlScripts().length < 1)
			return;

		for (String sqlScript : getSqlScripts()) {
			File sqlFile = File.createTempFile("sql", ".sql");
			FileUtil.copyResource(sqlScript, sqlFile);
			try (Connection con = getConnection()) {
				SqlFile sql = new SqlFile(sqlFile, "Cp1252", false);
				sql.setConnection(con);
				try {
					log.info("Running script {}", sqlScript);
					sql.execute();
				} catch (SqlToolError e) {
					throw new SQLException(e.getMessage(), e);
				}
			} finally {
				FileUtil.strongDelete(sqlFile);
			}
		}

		// Test the connection
		try (Connection con = getConnection();
				ResultSet rs = con.createStatement().executeQuery("select * from ld_menu where ld_id=2")) {
			rs.next();
			assertEquals(2, rs.getInt(1));
		}
	}
}