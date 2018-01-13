package com.logicaldoc.core.automation;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple utility class to give access to the system's log.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class LogTool {
	private static Logger log = LoggerFactory.getLogger(Automation.class);

	// Prints in the system output
	public void print(String message) {
		SimpleDateFormat df = new SimpleDateFormat("[yyyy-MM-dd HH:mm:ss]");
		System.out.println(df.format(new Date()) + " " + message);
	}

	public void debug(String message) {
		log.debug(message);
	}

	public void info(String message) {
		log.info(message);
	}

	public void warn(String message) {
		log.warn(message);
	}

	public void error(String message) {
		log.error(message);
	}
}