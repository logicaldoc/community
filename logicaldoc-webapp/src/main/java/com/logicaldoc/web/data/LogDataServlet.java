package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.jdom2.Element;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.util.config.LogConfigurator;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet is responsible for retrieving informations about the logs as the
 * list of appenders.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.2
 */
public class LogDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		try {
			ServletUtil.checkMenu(request, Menu.LOGS);
		} catch (Exception t) {
			ServletUtil.sendError(response, t.getMessage());
		}

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if (request.getParameter("loggers") != null)
			printLoggers(writer);
		else
			printAppenders(writer);

		writer.write("</list>");
	}

	private void printLoggers(PrintWriter writer) {
		LogConfigurator conf = new LogConfigurator();
		for (Element logger : conf.getLoggers().stream()
				.filter(l -> !"STORE_DELETIONS".equals(l.getAttributeValue("name")))
				.sorted((l1, l2) -> StringUtils.defaultString(l1.getAttributeValue("name"))
						.compareTo(StringUtils.defaultString(l2.getAttributeValue("name"))))
				.toList()) {
			String loggerName = StringUtils.defaultString(logger.getAttributeValue("name"), "root");
			writer.print("<logger><name><![CDATA[");
			writer.print(loggerName);
			writer.print("]]></name><level>");
			writer.print(StringUtils.defaultString(logger.getAttributeValue("level"), "debug"));
			writer.print("</level><additivity>");
			writer.print(StringUtils.defaultString(logger.getAttributeValue("additivity"), "true"));
			writer.print("</additivity><reserved>");
			writer.print(Boolean.toString(isLoggerReserved(loggerName)));
			writer.print("</reserved></logger>");
		}
	}

	private void printAppenders(PrintWriter writer) {
		LogConfigurator conf = new LogConfigurator();
		for (String appender : conf.getAppenders().stream().filter(a -> a.endsWith("_WEB")).toList()) {
			writer.print("<appender><name><![CDATA[");
			writer.print(appender);
			writer.print("]]></name><label><![CDATA[");
			writer.print(appender.replace("_WEB", ""));
			writer.print("]]></label></appender>");
		}
	}

	/**
	 * Checks if a logger is 'reserved' that means you cannot delete it and
	 * alter with restrictions
	 * 
	 * @param name name of the logger
	 * 
	 * @return if the given logger is reserved
	 */
	public static boolean isLoggerReserved(String name) {
		List<String> reservedNames = List.of("root", "STORE_DELETIONS");
		if (reservedNames.contains(name))
			return true;

		// Each Task logger is reserved
		try {
			Class<?> clazz = LogDataServlet.class.getClassLoader().loadClass(name);
			return Task.class.isAssignableFrom(clazz);
		} catch (Exception e) {
			// this may be normal
		}

		return false;
	}
}