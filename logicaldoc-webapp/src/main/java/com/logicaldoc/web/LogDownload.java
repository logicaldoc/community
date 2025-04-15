package com.logicaldoc.web;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringSubstitutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.SystemUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.LogConfigurator;
import com.logicaldoc.util.config.OrderedProperties;
import com.logicaldoc.util.csv.CSVFileWriter;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet grant access to log files
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class LogDownload extends HttpServlet {

	private static final String DOT_PROPERTIES = ".properties";

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(LogDownload.class);

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			ServletUtil.checkMenu(request, Menu.LOGS);
		} catch (Exception t) {
			ServletUtil.sendError(response, t.getMessage());
		}

		// Avoid resource caching
		response.setHeader("Cache-Control", "no-cache,no-store,must-revalidate");
		response.setHeader("Expires", "0");
		response.setHeader("Pragma", "no-cache");

		String appender = request.getParameter("appender");
		File file = null;
		try {
			if ("all".equals(appender)) {
				SimpleDateFormat df = new SimpleDateFormat("yyyyMMddHHmmss");

				response.setContentType("application/zip");
				response.setHeader("Content-Disposition",
						"attachment; filename=\"" + ("ldoc-log-" + df.format(new Date()) + ".zip") + "\"");

				file = prepareAllSupportResources();
			} else if (appender != null) {
				response.setContentType("text/html");
				LogConfigurator conf = new LogConfigurator();
				if (conf.getFile(appender, true) != null)
					file = new File(conf.getFile(appender, true));
			}

			if (file == null)
				return;

			downloadLogFile(response, appender, file);
		} catch (IOException | PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}

	private void downloadLogFile(HttpServletResponse response, String appender, File file) throws IOException {
		try (InputStream is = new BufferedInputStream(new FileInputStream(file));) {
			if (file.length() > 1) {
				response.setHeader("Content-Length", Long.toString(file.length()));
				byte[] buf = new byte[1024];
				int read = 1;

				while (read > 0) {
					read = is.read(buf, 0, buf.length);

					if (read > 0) {
						response.getOutputStream().write(buf, 0, read);
					}
				}
			} else {
				response.setHeader("Content-Length", "0");
				response.getWriter().println("");
			}
		} finally {
			if ("all".equals(appender) && file != null)
				FileUtils.deleteQuietly(file);
		}
	}

	/**
	 * Prepare all the support resources in one single zip: all the logs, the
	 * context.properties, the snapshot of the env variables.
	 * 
	 * @throws IOException error creting a temporary file
	 * @throws PersistenceException Error in the data layer
	 */
	private File prepareAllSupportResources() throws IOException, PersistenceException {
		File tmp = FileUtil.createTempFile("logs", ".zip");

		try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(tmp));) {
			/*
			 * Store the log files in the zip file
			 */
			LogConfigurator conf = new LogConfigurator();
			File logsDir = new File(conf.getLogsRoot());
			File[] files = logsDir.listFiles();

			// Sort by descending modified date
			Arrays.sort(files, Comparator.comparingLong(File::lastModified).reversed());

			// Take max 10 files of each type
			Map<String, Integer> counter = new HashMap<>();
			for (File file : files) {
				String baseName = StringUtils.substringBefore(FilenameUtils.getBaseName(file.getName()), ".");
				counter.computeIfAbsent(baseName, k -> Integer.valueOf(0));

				// store just the latest logs
				if (file.length() > 0 && counter.get(baseName) < 10
						&& !file.getName().toLowerCase().contains(".html")) {
					writeEntry(out, "logicaldoc/logs/" + file.getName(), file);
					counter.computeIfPresent(baseName, (k, v) -> v + 1);
				}
			}

			/*
			 * Now create a copy of the configuration and store it in the zip
			 * file
			 */
			OrderedProperties prop = writeContextPropertiesDump(out);

			/*
			 * Save the most important configuration files
			 */
			writeConfigFiles(out);

			/*
			 * Now create a file with the environment
			 */
			String env = SystemUtil.printEnvironment();
			env += "\n\n" + printDatabaseEnvironment();
			File buf = FileUtil.createTempFile("environment", ".txt");
			FileUtil.writeFile(env, buf.getPath());

			writeEntry(out, "logicaldoc/conf/environment.txt", buf);
			FileUtil.delete(buf);

			/*
			 * Discover the tomcat's folder
			 */
			ServletContext context = getServletContext();
			File webappFolder = new File(context.getRealPath("/WEB-INF/web.xml"));
			webappFolder = webappFolder.getParentFile().getParentFile().getParentFile().getParentFile();

			/*
			 * Now put the server.xml file and the other config files
			 */
			writeTomcatConfigFiles(out, webappFolder);

			/*
			 * Now put the most recent tomcat's logs
			 */
			writeTomcatLogs(out, webappFolder);

			/*
			 * Dump all the informations about updates and batches
			 */
			writeUpdateLogs(out);
			writePatchLogs(out);

			prop.store(new FileOutputStream(buf), "Support Request");
		}

		return tmp;
	}

	private String printDatabaseEnvironment() {
		DocumentDAO dao = (DocumentDAO) Context.get("documentDAO");
		Properties prop = new Properties();
		prop.putAll(dao.getDatabaseMetadata());

		StringWriter writer = new StringWriter();
		try {
			prop.store(new PrintWriter(writer), "Database Environment");
			return writer.getBuffer().toString();
		} catch (IOException e) {
			// Nothing to do
		}
		return "";
	}

	private void writeTomcatLogs(ZipOutputStream out, File webappDir) throws IOException {
		File logsDir = new File(webappDir.getPath() + "/logs");
		File[] files = logsDir.listFiles();

		// Sort by descending modified date
		Arrays.sort(files, Comparator.comparingLong(File::lastModified).reversed());

		// Take max 10 files of each type
		Map<String, Integer> counter = new HashMap<>();
		for (File file : files) {
			String baseName = StringUtils.substringBefore(FilenameUtils.getBaseName(file.getName()), ".");
			counter.computeIfAbsent(baseName, k -> Integer.valueOf(0));

			// store just the latest logs
			if (file.length() > 0 && counter.get(baseName) < 10) {
				writeEntry(out, "tomcat/logs/" + file.getName(), file);
				counter.computeIfPresent(file.getName(), (k, v) -> v + 1);
			}
		}
	}

	private void writeTomcatConfigFiles(ZipOutputStream out, File webappDir) throws IOException {
		File confDir = new File(webappDir.getPath() + "/conf");
		File[] files = confDir.listFiles();
		for (File file : files) {
			if (file.isDirectory())
				continue;

			// store just the logs of today
			if (file.getName().toLowerCase().endsWith(".xml") || file.getName().toLowerCase().endsWith(DOT_PROPERTIES)
					|| file.getName().toLowerCase().endsWith(".policy"))
				writeEntry(out, "tomcat/conf/" + file.getName(), file);
		}
	}

	private void writeConfigFiles(ZipOutputStream out) throws IOException {
		File confDir = new File(Context.get().getProperties().getProperty("LDOCHOME") + "/conf");
		File[] files = confDir.listFiles();
		for (File file : files) {
			if (file.isDirectory())
				continue;

			if (file.getName().toLowerCase().endsWith(DOT_PROPERTIES) || file.getName().toLowerCase().endsWith(".xml")
					|| file.getName().toLowerCase().endsWith(".txt"))
				writeEntry(out, "logicaldoc/conf/" + file.getName(), file);
		}
	}

	private void writeUpdateLogs(ZipOutputStream out) throws IOException, PersistenceException {
		/*
		 * Write he log files in the updates/ folder
		 */
		Properties buildProperties = loadBuildProperties();
		String dir = StringSubstitutor.replace(buildProperties.getProperty("update.dir"), buildProperties);
		File logsDir = new File(dir);
		File[] files = logsDir.listFiles();
		if (files != null)
			for (File file : files) {
				if (file.getName().toLowerCase().endsWith(".log"))
					writeEntry(out, "logicaldoc/updates/" + file.getName(), file);
			}

		/*
		 * Collect the updates table
		 */
		dumpUpdateTable(out);
	}

	private void dumpUpdateTable(ZipOutputStream out) throws IOException, PersistenceException {
		File buf = FileUtil.createTempFile("updates", ".csv");
		try {
			DocumentDAO dao = Context.get(DocumentDAO.class);
			dao.queryForResultSet("select ld_update, ld_date, ld_version from ld_update order by ld_date desc", null,
					null, rows -> {
						try (CSVFileWriter csv = new CSVFileWriter(buf.getAbsolutePath(), ',')) {
							SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
							while (rows.next()) {
								csv.writeFields(
										List.of(df.format(rows.getDate(2)), rows.getString(3), rows.getString(1)));
							}
						} catch (IOException e) {
							throw new PersistenceException(e);
						}
					});

			writeEntry(out, "logicaldoc/updates/updates.csv", buf);
		} finally {
			FileUtil.delete(buf);
		}
	}

	private void writePatchLogs(ZipOutputStream out) throws IOException, PersistenceException {
		/*
		 * Write he log files in the patches/ folder
		 */
		Properties buildProperties = loadBuildProperties();
		String dir = StringSubstitutor.replace(buildProperties.getProperty("patch.dir"), buildProperties);
		File logsDir = new File(dir);
		File[] files = logsDir.listFiles();
		if (files != null)
			for (File file : files) {
				if (file.getName().toLowerCase().endsWith(".log"))
					writeEntry(out, "logicaldoc/patches/" + file.getName(), file);
			}

		/*
		 * Collect the updates table
		 */
		dumpPatchTable(out);
	}

	private void dumpPatchTable(ZipOutputStream out) throws IOException, PersistenceException {
		File buf = FileUtil.createTempFile("patches", ".csv");
		try {
			DocumentDAO dao = Context.get(DocumentDAO.class);
			dao.queryForResultSet("select ld_patch, ld_date, ld_version from ld_patch order by ld_date desc", null,
					null, rows -> {
						try (CSVFileWriter csv = new CSVFileWriter(buf.getAbsolutePath(), ',')) {
							SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
							while (rows.next()) {
								csv.writeFields(
										List.of(df.format(rows.getDate(2)), rows.getString(3), rows.getString(1)));
							}
						} catch (IOException e) {
							throw new PersistenceException(e);
						}
					});

			writeEntry(out, "logicaldoc/patches/patches.csv", buf);
		} finally {
			FileUtil.delete(buf);
		}
	}

	private Properties loadBuildProperties() throws IOException {
		Properties buildProperties = new Properties();
		try (InputStream is = new FileInputStream(
				new File(Context.get().getProperties().getProperty("LDOCHOME") + "/conf/build.properties"))) {
			buildProperties.load(is);
		}
		return buildProperties;
	}

	/**
	 * Dumps the context properties as they are in memory
	 * 
	 * @param out The zip stream to write to
	 * 
	 * @return The written properties
	 * 
	 * @throws IOException error in writing the stream
	 */
	private OrderedProperties writeContextPropertiesDump(ZipOutputStream out) throws IOException {
		File buf = FileUtil.createTempFile("context", DOT_PROPERTIES);
		ContextProperties cp = Context.get().getProperties();
		OrderedProperties prop = new OrderedProperties();
		for (String key : cp.getKeys()) {
			if (!key.contains("password"))
				prop.put(key, cp.get(key));
		}
		prop.store(new FileOutputStream(buf), "Support Request");
		writeEntry(out, "logicaldoc/conf/context-dump.properties", buf);
		FileUtil.delete(buf);
		return prop;
	}

	public void writeEntry(ZipOutputStream out, String entry, File file) throws IOException {
		byte[] b;
		int count;

		try (FileInputStream in = new FileInputStream(file)) {
			// name the file inside the zip file
			out.putNextEntry(new ZipEntry(entry));

			// buffer size
			b = new byte[1024];
			count = 0;

			while ((count = in.read(b)) > 0)
				out.write(b, 0, count);
		}
	}

	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			doGet(request, response);
		} catch (Exception t) {
			// Nothing to do
		}
	}
}
