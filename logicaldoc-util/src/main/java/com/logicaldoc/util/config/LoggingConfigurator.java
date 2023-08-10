package com.logicaldoc.util.config;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.text.StrSubstitutor;
import org.jdom2.CDATA;
import org.jdom2.Element;

/**
 * Utility class for manipulating log4j.xml file
 * 
 * @author Michael Scholz, Marco Meschieri
 */
public class LoggingConfigurator {

	private static final String ROOT = "${root}/";

	private static final String FILE_NAME = "fileName";

	private static final String ROLLING_FILE_NAME = "//RollingFile[@name='";

	private static final String APPENDERS = "//Appenders";

	private XMLBean xml;

	public LoggingConfigurator() {
		URL configFile = null;
		try {
			configFile = LoggingConfigurator.class.getClassLoader().getResource("/log.xml");
		} catch (Exception t) {
			// Nothing to do
		}

		if (configFile == null)
			configFile = LoggingConfigurator.class.getClassLoader().getResource("log.xml");

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
	 * This method selects all the log files
	 * 
	 * @return collection of the log file paths
	 */
	public Collection<String> getLoggingFiles() {
		Collection<String> result = new ArrayList<>();
		Element appenders = xml.findElement(APPENDERS);
		List<Element> list = appenders.getChildren("RollingFile");
		Iterator<Element> iter = list.iterator();
		while (iter.hasNext()) {
			Element elem = iter.next();
			result.add(elem.getAttributeValue("name"));
		}
		return result;
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

	public void addLogger(String name, String[] appenders) {
		// Check category existence
		if (xml.findElement("//Logger[@name='" + name + "']") != null)
			return;

		Element logger = new Element("Logger");
		logger.setAttribute("name", name);
		logger.setAttribute("additivity", "false");
		logger.setAttribute("level", "info");

		for (int i = 0; i < appenders.length; i++) {
			Element appender = new Element("AppenderRef");
			appender.setAttribute("ref", appenders[i]);
			logger.addContent(appender);
		}

		// Place the new category just before the root category
		Element loggers = xml.findElement("//Loggers");
		loggers.addContent(0, logger);
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
}
