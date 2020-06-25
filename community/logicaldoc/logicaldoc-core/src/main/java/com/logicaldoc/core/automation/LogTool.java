package com.logicaldoc.core.automation;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple utility class to give access to the system's log from within the
 * Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
@AutomationDictionary(key = "log")
public class LogTool {
	private static Logger log = LoggerFactory.getLogger(Automation.class);

	/**
	 * Prints in the system output
	 * 
	 * @param message the message to print
	 */
	public void print(String message) {
		SimpleDateFormat df = new SimpleDateFormat("[yyyy-MM-dd HH:mm:ss]");
		System.out.println(df.format(new Date()) + " " + message);
	}

	/**
	 * Writes in the system log with DEBUG priority
	 * 
	 * @param message the message to print
	 */
	public void debug(String message) {
		log.debug(message);
	}

	/**
	 * Writes in the system log with INFO priority
	 * 
	 * @param message the message to print
	 */
	public void info(String message) {
		log.info(message);
	}

	/**
	 * Writes in the system log with WARN priority
	 * 
	 * @param message the message to print
	 */
	public void warn(String message) {
		log.warn(message);
	}

	/**
	 * Writes in the system log with ERROR priority
	 * 
	 * @param message the message to print
	 */
	public void error(String message) {
		log.error(message);
	}
}