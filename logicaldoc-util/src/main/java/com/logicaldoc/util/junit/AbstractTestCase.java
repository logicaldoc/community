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
import java.util.concurrent.TimeUnit;

import javax.sql.DataSource;

import org.apache.commons.io.FilenameUtils;
import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlToolError;
import org.junit.After;
import org.junit.AssumptionViolatedException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.Stopwatch;
import org.junit.runner.Description;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.util.CollectionUtils;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.time.Pause;
import com.logicaldoc.util.time.TimeDiff;

import jakarta.persistence.PersistenceException;

/**
 * Abstract test case that of database and context initialization.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.8.5
 */
public abstract class AbstractTestCase {

	private static final Logger log = LoggerFactory.getLogger(AbstractTestCase.class);

	private static final String USER_HOME = "user.home";

	protected ApplicationContext context;

	protected File tempDir = new File("target/tmp");

	protected final String originalUserHome = System.getProperty(USER_HOME);

	/**
	 * Utility method to print the time spent executing the current test
	 * 
	 * @param description the current execution description as provided by JUnit
	 * @param status How the test completed(eg succeeded, failed, finished)
	 * @param nanos The time spent in nanoseconds
	 */
	protected static void logDuration(Description description, String status, long nanos) {
		String testName = description.getClassName() + "#" + description.getMethodName();
		String message = String.format("Test %s %s, spent %s", testName, status,
				TimeDiff.printDuration(TimeUnit.NANOSECONDS.toMillis(nanos)));
		switch (status) {
			case "failed" -> log.error(message);
			case "succeeded" -> log.debug(message);
			case "skipped" -> log.debug(message);
			default -> log.info(message);
		}
	}

	@Rule
	public Stopwatch stopwatch = new Stopwatch() {
		@Override
		protected void succeeded(long nanos, Description description) {
			logDuration(description, "succeeded", nanos);
		}

		@Override
		protected void failed(long nanos, Throwable e, Description description) {
			logDuration(description, "failed", nanos);
		}

		@Override
		protected void skipped(long nanos, AssumptionViolatedException e, Description description) {
			logDuration(description, "skipped", nanos);
		}

		@Override
		protected void finished(long nanos, Description description) {
			logDuration(description, "finished", nanos);
		}
	};

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		System.setProperty("LOGICALDOC_REPOSITORY", "target");
		System.setProperty("java.io.tmpdir", tempDir.getAbsolutePath());

		try {
			loadDevelSettingsInEnvironment();

			updateUserHome();

			createTestDirs();

			initializePlugins(getPluginArchives());

			context = buildApplicationContext();

			loadDevelSettingsInContext();

			createDatabase();
		} catch (Exception e) {
			restoreUserHome();
			log.error(e.getMessage(), e);

			switch (e) {
				case IOException ioe -> throw ioe;
				case SQLException sqe -> throw sqe;
				case PersistenceException pe -> throw pe;
				default -> throw new PluginException(e);
			}
		}
	}

	@After
	public void tearDown() throws IOException {
		try {
			destroyDatabase();
			File pluginsDir = new File(
					Context.get().getProperties().getProperty("conf.plugindir", "target/tests-plugins"));
			FileUtil.delete(pluginsDir);

			if (context != null)
				((AbstractApplicationContext) context).close();
		} catch (Exception e) {
			// Ignore
		} finally {
			restoreUserHome();
		}
	}

	private void updateUserHome() {
		System.setProperty(USER_HOME, tempDir.getPath());
	}

	/**
	 * Concrete implementations should prepare and return the Spring
	 * {@link ApplicationContext} to use. By default it is used the
	 * {@link AnnotationConfigApplicationContext}.
	 * 
	 * @return the ApplicationContext
	 */
	protected ApplicationContext buildApplicationContext() {
		return new AnnotationConfigApplicationContext(Context.class);
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
	protected void initializePlugins(List<String> pluginArchives) throws IOException, PluginException {
		if (CollectionUtils.isEmpty(pluginArchives))
			return;

		File pluginsDir = new File(new ContextProperties().getProperty("conf.plugindir", "target/tests-plugins"));
		FileUtil.delete(pluginsDir);
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
					log.trace("Running script {}", sqlScript);
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
	private void loadDevelSettingsInEnvironment() throws IOException {
		Properties devSettings = new Properties();
		try (FileReader reader = new FileReader(new File(originalUserHome + "/logicaldoc-dev.properties"))) {
			devSettings.load(reader);
			for (Map.Entry<Object, Object> entry : devSettings.entrySet())
				System.setProperty(entry.getKey().toString(), entry.getValue().toString());
		}
	}

	/**
	 * Loads the settigs in the file user.home/logicaldoc-dev.properties putting
	 * them as context settings
	 * 
	 * @throws IOException Error reading the development file
	 */
	private void loadDevelSettingsInContext() throws IOException {
		Properties devSettings = new Properties();
		try (FileReader reader = new FileReader(new File(originalUserHome + "/logicaldoc-dev.properties"))) {
			devSettings.load(reader);
			for (Map.Entry<Object, Object> entry : devSettings.entrySet())
				Context.get().getProperties().setProperty(entry.getKey().toString(), entry.getValue().toString());
		}
	}

	protected void createTestDirs() {
		FileUtil.delete(tempDir);
		tempDir.mkdirs();
	}

	private void restoreUserHome() {
		// Restore user home system property
		System.setProperty(USER_HOME, originalUserHome);
	}

	/**
	 * Destroys the in-memory database
	 */
	private void destroyDatabase() {
		if (CollectionUtils.isEmpty(getDatabaseScripts()))
			return;

		try (Connection con = getConnection(); Statement statement = con.createStatement()) {
			statement.execute("shutdown");
		} catch (SQLException e) {
			// Ignore
		}
	}

	protected Connection getConnection() throws SQLException {
		return Context.get(DataSource.class).getConnection();
	}

	protected void waiting() throws InterruptedException {
		Pause.doPause(5000L);
	}
}