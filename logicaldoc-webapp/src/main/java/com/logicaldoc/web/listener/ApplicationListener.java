package com.logicaldoc.web.listener;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Enumeration;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configurator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.LoggingConfigurator;
import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.config.WebContextConfigurator;
import com.logicaldoc.util.dbinit.DBInit;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Listener that initializes relevant system stuffs during application startup
 * and session life cycle.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class ApplicationListener implements ServletContextListener, HttpSessionListener {

	private static final String JAVA_IO_TMPDIR = "java.io.tmpdir";

	private static final String JDBC_URL = "jdbc.url";

	private static Logger log = LoggerFactory.getLogger(ApplicationListener.class);

	private boolean pidCreated = false;

	private static boolean restartRequired = false;

	public static boolean isRestartRequired() {
		return restartRequired;
	}

	public static void restartRequired() {
		ApplicationListener.restartRequired = true;
	}

	@Override
	public void contextDestroyed(ServletContextEvent sce) {
		try {
			onShutdown();
		} finally {
			if (pidCreated)
				FileUtil.strongDelete(getPidFile());
		}
	}

	public static void onShutdown() {
		log.warn("Shutting down the application");

		/*
		 * Try to shutdown the DB connection
		 */
		try {
			ContextProperties config = new ContextProperties();
			if (config.getProperty(JDBC_URL).contains("jdbc:hsqldb")) {
				DBInit dbInit = new DBInit();
				dbInit.setDriver(config.getProperty("jdbc.driver"));
				dbInit.setUrl(config.getProperty(JDBC_URL));
				dbInit.setUsername(config.getProperty("jdbc.username"));
				dbInit.setPassword(config.getProperty("jdbc.password"));

				dbInit.executeSql("shutdown compact");
				log.warn("Embedded database stopped");
			} else if (config.getProperty(JDBC_URL).contains("jdbc:mysql")) {
				com.mysql.cj.jdbc.AbandonedConnectionCleanupThread.uncheckedShutdown();
			}
		} catch (Exception e) {
			log.warn(e.getMessage());
		}

		/*
		 * De-register the database drivers
		 */
		try {
			Enumeration<Driver> drivers = DriverManager.getDrivers();
			Driver d = null;
			while (drivers.hasMoreElements()) {
				d = drivers.nextElement();
				registerDriver(d);
			}
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}

		Set<Thread> threadSet = Thread.getAllStackTraces().keySet();
		Thread[] threadArray = threadSet.toArray(new Thread[threadSet.size()]);

		for (Thread t : threadArray) {
			synchronized (t) {
				if ((t.getName().toLowerCase().contains("abandoned connection") || t.getName().startsWith("Scheduler_"))
						&& !Thread.currentThread().equals(t) && !t.isInterrupted())
					interruptThread(t);
			}
		}

		log.warn("Application stopped");
	}

	private static void interruptThread(Thread thread) {
		try {
			thread.interrupt();
			log.warn("Killed thread {}", thread.getName());
		} catch (Exception e) {
			log.warn("Error killing {}", thread.getName());
		}
	}

	private static void registerDriver(Driver driver) {
		try {
			DriverManager.deregisterDriver(driver);
			if (log.isWarnEnabled())
				log.warn(String.format("Driver %s unregistered", driver.getClass().getName()));
		} catch (SQLException ex) {
			log.warn(String.format("Error unregistering driver %s", driver), ex);
		}
	}

	@Override
	public void contextInitialized(ServletContextEvent sce) {
		try {
			ServletContext context = sce.getServletContext();

			// Initialize logging
			initializeLogging();

			// Update the web descriptor with the correct transport guarantee
			try {
				ContextProperties conf = new ContextProperties();
				String policy = "true".equals(conf.getProperty("ssl.required")) ? "CONFIDENTIAL" : "NONE";
				WebConfigurator configurator = new WebConfigurator(context.getRealPath("/WEB-INF/web.xml"));
				if (configurator.setTransportGuarantee(policy)) {
					PluginRegistry.getInstance().setRestartRequired();
				}
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}

			// Update the cookies processor with the correct sameSite
			try {
				ContextProperties conf = new ContextProperties();
				WebContextConfigurator configurator = new WebContextConfigurator(
						context.getRealPath("/META-INF/context.xml"));
				if (configurator.setSameSiteCookies(conf.getProperty("cookies.samesite", "lax"))) {
					PluginRegistry.getInstance().setRestartRequired();
				}
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}

			// Prepare the plugins dir
			String pluginsDir = context.getRealPath("/WEB-INF/lib");

			// Initialize plugins
			try {
				PluginRegistry.getInstance().init(pluginsDir);
			} catch (PluginException e) {
				log.error(e.getMessage(), e);
				return;
			}

			// Clean some temporary folders
			cleanTemporaryFolders(context);

			// Initialize the Automation
			Automation.initialize();

			// Try to unpack new plugins
			try {
				unpackPlugins(context);
			} catch (IOException e) {
				log.warn(e.getMessage());
			}

			if (PluginRegistry.getInstance().isRestartRequired()) {
				restartRequired();
				log.warn("The application has to be restarted");
				System.out.println("The application has to be restarted");
			}
		} finally {
			writePidFile();
		}
	}

	private void cleanTemporaryFolders(ServletContext context) {
		File tempDirToDelete = new File(context.getRealPath("upload"));
		try {
			if (tempDirToDelete.exists())
				FileUtils.forceDelete(tempDirToDelete);
		} catch (IOException e) {
			log.warn(e.getMessage());
		}

		tempDirToDelete = new File(System.getProperty(JAVA_IO_TMPDIR) + "/upload");
		try {
			if (tempDirToDelete.exists())
				FileUtils.forceDelete(tempDirToDelete);
		} catch (IOException e) {
			log.warn(e.getMessage());
		}

		tempDirToDelete = new File(System.getProperty(JAVA_IO_TMPDIR) + "/convertjpg");
		try {
			if (tempDirToDelete.exists())
				FileUtils.forceDelete(tempDirToDelete);
		} catch (IOException e) {
			log.warn(e.getMessage());
		}
	}

	private void initializeLogging() {
		String log4jPath = null;
		try {
			log4jPath = getLogConfigFilePath();

			// Setup the correct logs folder
			ContextProperties config = new ContextProperties();
			LoggingConfigurator lconf = new LoggingConfigurator();
			lconf.setLogsRoot(config.getProperty("conf.logdir"));
			lconf.write();

			// Init the logs
			System.out.println(String.format("Taking log configuration from %s", log4jPath));
			LoggerContext lContext = Configurator.initialize(null, log4jPath);
			if (lContext == null)
				throw new IOException("Null logger context");
		} catch (Exception e) {
			System.err.println(String.format("Cannot initialize the log: %s", e.getMessage()));
		}
	}

	private String getLogConfigFilePath() throws UnsupportedEncodingException {
		String log4jPath;
		URL configFile = null;
		try {
			configFile = LoggingConfigurator.class.getClassLoader().getResource("/log.xml");
		} catch (Exception t) {
			// Nothing to do
		}

		if (configFile == null)
			configFile = LoggingConfigurator.class.getClassLoader().getResource("log.xml");

		log4jPath = URLDecoder.decode(configFile.getPath(), "UTF-8");
		return log4jPath;
	}

	/**
	 * Unpacks the plugin that are newly installed and positioned in the plugins
	 * folder
	 * 
	 * @throws IOException
	 */
	private void unpackPlugins(ServletContext context) throws IOException {
		File pluginsDir = PluginRegistry.getPluginsDir();
		File[] archives = pluginsDir.listFiles((dir, fileName) -> {
			return fileName.toLowerCase().contains("plugin") && fileName.toLowerCase().endsWith(".zip");
		});
		File webappDir = new File(context.getRealPath("/"));
		if (archives != null)
			for (File archive : archives) {
				log.info("Unpack plugin {}", archive.getName());
				ZipUtil zipUtil = new ZipUtil();
				zipUtil.unzip(archive.getPath(), webappDir.getPath());

				// Delete the plugin.xml file
				FileUtils.deleteQuietly(new File(webappDir, "plugin.xml"));

				FileUtils.moveFile(archive, new File(archive.getParentFile(), archive.getName() + ".installed"));

				String pluginName = archive.getName().replace("-plugin.zip", "");
				pluginName = pluginName.substring(0, pluginName.lastIndexOf('-'));
				log.info("Remove installation marker of plugin {}", pluginName);
				File pluginDir = new File(pluginsDir, pluginName);
				File[] installationMarkers = pluginDir.listFiles((File dir, String fileName) -> {
					return fileName.toLowerCase().startsWith("install-");
				});
				for (File file : installationMarkers)
					FileUtils.deleteQuietly(file);

				restartRequired();
			}
	}

	@Override
	public void sessionCreated(HttpSessionEvent event) {
		// Nothing to do
	}

	/**
	 * Frees temporary upload folders.
	 */
	@Override
	public void sessionDestroyed(HttpSessionEvent event) {
		HttpSession session = event.getSession();
		String id = session.getId();

		// Remove the upload folders
		File uploadFolder = new File(session.getServletContext().getRealPath("upload"));
		uploadFolder = new File(uploadFolder, id);
		try {
			if (uploadFolder.exists())
				FileUtils.forceDelete(uploadFolder);
		} catch (Exception e) {
			log.warn(e.getMessage());
		}

		String sid = (String) session.getAttribute("sid");
		File uploadDir = new File(System.getProperty(JAVA_IO_TMPDIR) + "/upload/" + sid);
		try {
			if (uploadDir.exists())
				FileUtils.forceDelete(uploadDir);
		} catch (IOException e) {
			log.warn(e.getMessage());
		}
	}

	private void writePidFile() {
		File pidFile = getPidFile();
		if (pidFile.exists())
			return;
		long pid = ProcessHandle.current().pid();
		try {
			FileUtils.touch(pidFile);
			FileUtil.writeFile("" + pid, pidFile.getPath());
			pidCreated = true;
		} catch (IOException e) {
			log.warn(e.getMessage());
			System.err.println("Cannot create pid file " + pidFile.getAbsolutePath());
		}
	}

	private File getPidFile() {
		try {
			ContextProperties config = new ContextProperties();
			return new File(config.getProperty("LDOCHOME") + "/bin/pid");
		} catch (Exception t) {
			return new File("pid");
		}
	}
}