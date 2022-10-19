package com.logicaldoc.web;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
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

import com.logicaldoc.core.security.Menu;
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

	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			ServletUtil.checkMenu(request, Menu.LOGS);
		} catch (Throwable t) {
			ServletUtil.sendError(response, t.getMessage());
		}

		String appender = request.getParameter("appender");
		File file = null;

		try {
			if ("all".equals(appender)) {
				SimpleDateFormat df = new SimpleDateFormat("yyyyMMddHHmmss");

				response.setContentType("application/zip");
				response.setHeader("Content-Disposition",
						"attachment; filename=\"" + ("ldoc-log-" + df.format(new Date()) + ".zip") + "\"");

				// Avoid resource caching
				response.setHeader("Pragma", "no-cache");
				response.setHeader("Cache-Control", "no-store");
				response.setDateHeader("Expires", 0);

				file = prepareAllSupportResources(request, response);
			} else if (appender != null) {
				response.setContentType("text/html");
				LoggingConfigurator conf = new LoggingConfigurator();
				if (conf.getFile(appender, true) != null)
					file = new File(conf.getFile(appender, true));
			}

			if (file == null)
				return;

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
			} catch (Throwable ex) {
				// Nothing to do
			} finally {
				if ("all".equals(appender) && file != null)
					FileUtils.deleteQuietly(file);
			}
		} catch (Throwable ex) {
			log.warn(ex.getMessage(), ex);
		}
	}

	/**
	 * Prepare all the support resources in one single zip: all the logs, the
	 * context.properties, the snapshot of the env variables.
	 */
	private File prepareAllSupportResources(HttpServletRequest request, HttpServletResponse response) {
		File tmp = null;

		try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(tmp));) {
			tmp = File.createTempFile("logs", ".zip");

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
			File buf = File.createTempFile("context", ".properties");
			ContextProperties cp = Context.get().getProperties();
			OrderedProperties prop = new OrderedProperties();
			for (String key : cp.getKeys()) {
				if (key.contains("password"))
					continue;
				else
					prop.put(key, cp.get(key));
			}
			prop.store(new FileOutputStream(buf), "Support Request");

			writeEntry(out, "context.properties", buf);
			FileUtil.strongDelete(buf);

			/*
			 * Now create a file with the environment
			 */
			String env = SystemUtil.printEnvironment();
			buf = File.createTempFile("environment", ".txt");
			FileUtil.writeFile(env, buf.getPath());
			writeEntry(out, "environment.txt", buf);
			FileUtil.strongDelete(buf);

			/*
			 * Discover the tomcat's folder
			 */
			ServletContext context = getServletContext();
			File tomcatFile = new File(context.getRealPath("/WEB-INF/web.xml"));
			tomcatFile = tomcatFile.getParentFile().getParentFile().getParentFile().getParentFile();

			/*
			 * Now put the server.xml file
			 */
			File serverFile = new File(tomcatFile.getPath() + "/conf/server.xml");
			writeEntry(out, "tomcat/server.xml", serverFile);

			/*
			 * Now put the most recent tomcat's logs
			 */
			SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
			String today = df.format(new Date());
			File logsDir = new File(tomcatFile.getPath() + "/logs");
			File[] files = logsDir.listFiles();
			for (File file : files) {
				if (file.isDirectory() || (!file.getName().toLowerCase().endsWith(".log")
						&& !file.getName().toLowerCase().endsWith(".out")
						&& !file.getName().toLowerCase().endsWith(".txt")))
					continue;

				// store just the logs of today
				if (file.getName().toLowerCase().endsWith(".out")
						|| file.getName().toLowerCase().endsWith(today + ".log")
						|| file.getName().toLowerCase().endsWith(today + ".txt"))
					writeEntry(out, "tomcat/" + file.getName(), file);
			}

			prop.store(new FileOutputStream(buf), "Support Request");
		} catch (Throwable ex) {
			log.error(ex.getMessage(), ex);
		}

		return tmp;
	}

	public void writeEntry(ZipOutputStream out, String entry, File file) throws FileNotFoundException, IOException {

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

	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			doGet(request, response);
		} catch (Throwable t) {
		}
	}
}
