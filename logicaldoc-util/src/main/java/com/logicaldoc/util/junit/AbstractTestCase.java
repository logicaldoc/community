package com.logicaldoc.util.junit;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import org.apache.commons.io.FilenameUtils;
import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlToolError;
import org.junit.After;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.util.CollectionUtils;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.time.Pause;

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

	private String userHome = System.getProperty(USER_HOME);

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		loadDevelSettings();

		updateUserHome();

		createTestDirs();

		initializePlugins();

		context = buildApplicationContext();

		createDatabase();
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
		System.setProperty(USER_HOME, tempDir.getPath());
	}

	/**
	 * Concrete implementations should prepare and return the Spring
	 * ApplicationContext to use. By default it is used the
	 * ClassPathXmlApplicationContext loading the /context.xml resource.
	 * 
	 * @return the ApplicationContext
	 */
	protected ApplicationContext buildApplicationContext() {
		return new ClassPathXmlApplicationContext("/context.xml");
	}

	/**
	 * Concrete implementations should return the list of plugin archives to
	 * initialize
	 * 
	 * @return collection of plugin archives
	 */
	protected List<String> getPluginArchives() {
		return new ArrayList<>();
	}

	/**
	 * Initializes the declared plugins
	 * 
	 * @throws IOException I/O error retrieving the plugin archives
	 * @throws PluginException Error during plugin initialization
	 */
	protected void initializePlugins() throws IOException, PluginException {
		List<String> pluginArchives = getPluginArchives();
		if (CollectionUtils.isEmpty(pluginArchives))
			return;

		File pluginsDir = new File("target/tests-plugins");
		pluginsDir.mkdir();

		for (String pluginArchive : pluginArchives) {
			File pluginFile = new File(pluginsDir, FilenameUtils.getName(pluginArchive));
			FileUtil.copyResource(pluginArchive, pluginFile);
		}

		PluginRegistry registry = PluginRegistry.getInstance();
		registry.init(pluginsDir.getAbsolutePath());
	}

	/**
	 * Concrete implementations should return the array of sql script resources
	 * to use to setup the database
	 * 
	 * @return array of resources(database script files)
	 */
	protected List<String> getDatabaseScripts() {
		return new ArrayList<>();
	}

	/**
	 * Creates an in-memory test database
	 * 
	 * @throws SQLException Error in one of the SQL scripts
	 * @throws IOException Error reading one of the SQL scripts
	 */
	private void createDatabase() throws SQLException, IOException {
		final List<String> databaseScripts = getDatabaseScripts();
		if (CollectionUtils.isEmpty(databaseScripts))
			return;

		for (String sqlScript : databaseScripts) {
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
				FileUtil.delete(sqlFile);
			}
		}

		// Test the connection
		try (Connection con = getConnection(); ResultSet rs = con.createStatement().executeQuery("CALL NOW()")) {
			rs.next();
			assertNotNull(rs.getObject(1));
		}
	}

	/**
	 * Loads the settigs in the file user.home/logicaldoc-dev.properties putting
	 * them as Java variables
	 * 
	 * @throws IOException Error reading the development file
	 */
	private void loadDevelSettings() throws IOException {
		Properties devSettings = new Properties();
		try (FileReader reader = new FileReader(new File(userHome + "/logicaldoc-dev.properties"))) {
			devSettings.load(reader);
			for (Map.Entry<Object, Object> entry : devSettings.entrySet())
				System.setProperty(entry.getKey().toString(), entry.getValue().toString());
		}
	}

	protected void createTestDirs() {
		FileUtil.delete(tempDir);
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
		if (CollectionUtils.isEmpty(getDatabaseScripts()))
			return;

		try (Connection con = getConnection(); Statement statement = con.createStatement()) {
			statement.execute("shutdown");
		}
	}

	protected Connection getConnection() throws SQLException {
		DataSource ds = (DataSource) context.getBean("DataSource");
		return ds.getConnection();
	}

	protected void waiting() throws InterruptedException {
		Pause.doPause(5000L);
	}
}