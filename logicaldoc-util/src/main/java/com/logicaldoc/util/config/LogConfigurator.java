package com.logicaldoc.util.config;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang.text.StrSubstitutor;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configurator;
import org.jdom2.CDATA;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class for manipulating log4j.xml file
 * 
 * @author Michael Scholz, Marco Meschieri
 */
public class LogConfigurator {

	private static final String LEVEL = "level";

	private static final String LOGGERS = "//Loggers";

	private static final String ROOT = "${root}/";

	private static final String FILE_NAME = "fileName";

	private static final String ROLLING_FILE_NAME = "//RollingFile[@name='";

	private static final String APPENDERS = "//Appenders";

	private XMLBean xml;

	public LogConfigurator() {
		URL configFile = null;
		try {
			configFile = LogConfigurator.class.getClassLoader().getResource("/log.xml");
		} catch (Exception t) {
			// Nothing to do
		}

		if (configFile == null)
			configFile = LogConfigurator.class.getClassLoader().getResource("log.xml");

		xml = new XMLBean(configFile);
	}

	public String getProperty(String name) {
		Element elem = xml.findElement("//Property[@name='" + name + "']");
		if (elem != null)
			return elem.getTextTrim();
		else
			return null;
	}

	/**
	 * This method selects all the appender names
	 * 
	 * @return collection of the log appender names
	 */
	public Collection<String> getAppenders() {
		Element appenders = xml.findElement(APPENDERS);
		List<Element> children = appenders.getChildren();
		return children.stream().map(c -> c.getAttributeValue("name")).toList();
	}

	/**
	 * This method selects all the logger names
	 * 
	 * @return collection of the logger names
	 */
	public Collection<Element> getLoggers() {
		Element loggers = xml.findElement(LOGGERS);
		return loggers.getChildren();
	}

	/**
	 * Same as {@link #getFile(String, boolean)}
	 * 
	 * @param appender name of the appender
	 * 
	 * @return the path of the log file
	 */
	public String getFile(String appender) {
		return getFile(appender, true);
	}

	/**
	 * This method selects a filepath of an appender.
	 * 
	 * @param appender The appender name
	 * @param replaceVariables If true all variables(${var}) in the file path
	 *        will be substituted
	 * 
	 * @return The log file path
	 */
	public String getFile(String appender, boolean replaceVariables) {
		String result = null;
		Element elem = xml.findElement(ROLLING_FILE_NAME + appender + "']");
		if (elem != null) {
			result = elem.getAttributeValue(FILE_NAME);
			if (result.contains("${root}"))
				result = result.replace("${root}", getProperty("root"));

			if (replaceVariables) {
				result = StrSubstitutor.replaceSystemProperties(result);
			}
		}

		return result;
	}

	public void addTextAppender(String name) {
		if (xml.findElement(ROLLING_FILE_NAME + name + "']") != null)
			return;

		// Get the DMS appender and use it as a model
		Element model = xml.findElement("//RollingFile[@name='DMS']");

		// Clone the model and add to the appenders
		Element newAppender = model.clone();
		Element appenders = xml.findElement(APPENDERS);
		appenders.addContent(0, newAppender);

		// Setup the appender name
		newAppender.setAttribute("name", name);

		// Now setup the file name
		String logfile = name.trim().toLowerCase() + ".log";
		newAppender.setAttribute(FILE_NAME, ROOT + logfile);
		newAppender.setAttribute("filePattern", ROOT + logfile + ".%i");
	}

	public void addHtmlAppender(String name) {
		if (xml.findElement(ROLLING_FILE_NAME + name + "']") != null)
			return;

		// Get the DMS appender and use it as a model
		Element model = xml.findElement("//RollingFile[@name='DMS_WEB']");

		// Clone the model and add to the appenders
		Element newAppender = model.clone();
		Element appenders = xml.findElement(APPENDERS);
		appenders.addContent(0, newAppender);

		// Setup the appender name
		newAppender.setAttribute("name", name);

		// Now setup the file name
		String logfile = name.trim().toLowerCase() + ".log.html";
		newAppender.setAttribute(FILE_NAME, ROOT + logfile);
		newAppender.setAttribute("filePattern", ROOT + logfile + ".%i");
	}

	public void addLogger(String name, List<String> appenders) {
		setLogger(name, false, "info", appenders);
	}

	/**
	 * Adds or modifies a logger
	 * 
	 * @param name the logger name
	 * @param additivity the additivity flag
	 * @param level the logger level
	 * @param appenders optional list of appenders to assign
	 */
	public void setLogger(String name, boolean additivity, String level, List<String> appenders) {
		// Check logger existence
		Element logger = xml.findElement("//Logger[@name='" + name + "']");
		if (logger == null) {
			logger = new Element("Logger");
			logger.setAttribute("name", name);

			// Place the new logger just before the root category
			Element loggers = xml.findElement(LOGGERS);
			loggers.addContent(0, logger);
		}

		logger.setAttribute("additivity", Boolean.toString(additivity));
		logger.setAttribute(LEVEL, level);
		if (appenders != null && !appenders.isEmpty()) {
			logger.removeContent();
			for (String app : appenders) {
				Element appender = new Element("AppenderRef");
				appender.setAttribute("ref", app);
				logger.addContent(appender);
			}
		}
	}

	/**
	 * Removes a logger
	 * 
	 * @param name the logger name
	 */
	public void removeLogger(String name) {
		// Check logger existence
		Element logger = xml.findElement("//Logger[@name='" + name + "']");
		if (logger != null) {
			// Place the new logger just before the root category
			Element loggers = xml.findElement(LOGGERS);
			loggers.removeContent(logger);
		}
	}

	public String getRootLevel() {
		return xml.findElement("//Root").getAttributeValue(LEVEL);
	}

	public void setRootLevel(String level) {
		xml.findElement("//Root").setAttribute(LEVEL, level);
	}

	/**
	 * Sets a common path for all file appenders.
	 * 
	 * @param rootPath The path to be used
	 */
	public void setLogsRoot(String rootPath) {
		Element elem = xml.findElement("//Property[@name='root']");
		if (elem != null)
			elem.setContent(new CDATA(rootPath.trim()));
	}

	public String getLogsRoot() {
		return getProperty("root");
	}

	public boolean write() {
		return xml.writeXMLDoc();
	}

	public void initializeLogging() {
		Logger log = LoggerFactory.getLogger("console");
		String log4jPath = null;
		try {
			log4jPath = getLogConfigFilePath();

			// Setup the correct logs folder
			ContextProperties config = new ContextProperties();
			LogConfigurator lconf = new LogConfigurator();
			lconf.setLogsRoot(config.getProperty("conf.logdir"));
			lconf.write();

			// Init the logs
			log.info("Initializing the logging taking configuration from {}", log4jPath);
			LoggerContext lContext = Configurator.initialize(null, log4jPath);
			if (lContext == null)
				throw new IOException("Null logger context");
		} catch (Exception e) {
			log.error("Cannot initialize the log: {}", e.getMessage());
		}
	}

	private String getLogConfigFilePath() throws UnsupportedEncodingException {
		String log4jPath;
		URL configFile = null;
		try {
			configFile = LogConfigurator.class.getClassLoader().getResource("/log.xml");
		} catch (Exception t) {
			// Nothing to do
		}

		if (configFile == null)
			configFile = LogConfigurator.class.getClassLoader().getResource("log.xml");

		log4jPath = URLDecoder.decode(configFile.getPath(), "UTF-8");
		return log4jPath;
	}
}