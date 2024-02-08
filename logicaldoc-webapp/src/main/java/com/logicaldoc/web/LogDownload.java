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
import java.util.Collection;
import java.util.Date;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.SystemUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.LoggingConfigurator;
import com.logicaldoc.util.config.OrderedProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet grant access to log files
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class LogDownload extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(LogDownload.class);

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
				LoggingConfigurator conf = new LoggingConfigurator();
				if (conf.getFile(appender, true) != null)
					file = new File(conf.getFile(appender, true));
			}

			if (file == null)
				return;

			downloadLogFile(response, appender, file);
		} catch (IOException e) {
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
	 */
	private File prepareAllSupportResources() throws IOException {
		File tmp = FileUtil.createTempFile("logs", ".zip");

		try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(tmp));) {

			LoggingConfigurator conf = new LoggingConfigurator();
			Collection<String> appenders = conf.getLoggingFiles();

			/*
			 * Store the log files in the zip file
			 */
			for (String appender : appenders) {
				if (appender.endsWith("_WEB"))
					continue;

				File logFile = new File(conf.getFile(appender, true));
				writeEntry(out, logFile.getName(), logFile);
			}

			/*
			 * Now create a copy of the configuration and store it in the zip
			 * file
			 */
			File buf;
			OrderedProperties prop = writeContextProperties(out);

			/*
			 * Now create a file with the environment
			 */
			String env = SystemUtil.printEnvironment();
			env += "\n\n" + printDatabaseEnvironment();
			buf = FileUtil.createTempFile("environment", ".txt");
			FileUtil.writeFile(env, buf.getPath());

			writeEntry(out, "environment.txt", buf);
			FileUtil.strongDelete(buf);

			/*
			 * Discover the tomcat's folder
			 */
			ServletContext context = getServletContext();
			File webappDescriptor = new File(context.getRealPath("/WEB-INF/web.xml"));
			webappDescriptor = webappDescriptor.getParentFile().getParentFile().getParentFile().getParentFile();

			/*
			 * Now put the server.xml file
			 */
			File serverFile = new File(webappDescriptor.getPath() + "/conf/server.xml");
			writeEntry(out, "tomcat/server.xml", serverFile);

			/*
			 * Now put the most recent tomcat's logs
			 */
			writeTomcatLogs(out, webappDescriptor);

			prop.store(new FileOutputStream(buf), "Support Request");
		}

		return tmp;
	}

	private String printDatabaseEnvironment() {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean("DocumentDAO");
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

	private void writeTomcatLogs(ZipOutputStream out, File webappDescriptor) throws IOException {
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		String today = df.format(new Date());
		File logsDir = new File(webappDescriptor.getPath() + "/logs");
		File[] files = logsDir.listFiles();
		for (File file : files) {
			if (file.isDirectory()
					|| (!file.getName().toLowerCase().endsWith(".log") && !file.getName().toLowerCase().endsWith(".out")
							&& !file.getName().toLowerCase().endsWith(".txt")))
				continue;

			// store just the logs of today
			if (file.getName().toLowerCase().endsWith(".out") || file.getName().toLowerCase().endsWith(today + ".log")
					|| file.getName().toLowerCase().endsWith(today + ".txt"))
				writeEntry(out, "tomcat/" + file.getName(), file);
		}
	}

	private OrderedProperties writeContextProperties(ZipOutputStream out) throws IOException {
		File buf = FileUtil.createTempFile("context", ".properties");
		ContextProperties cp = Context.get().getProperties();
		OrderedProperties prop = new OrderedProperties();
		for (String key : cp.getKeys()) {
			if (!key.contains("password"))
				prop.put(key, cp.get(key));
		}
		prop.store(new FileOutputStream(buf), "Support Request");

		writeEntry(out, "context.properties", buf);
		FileUtil.strongDelete(buf);
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
