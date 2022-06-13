package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskManager;
import com.logicaldoc.core.task.TaskTrigger;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for tasks data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TasksDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TasksDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);
			String localeStr = request.getParameter("locale");
			Locale locale = LocaleUtil.toLocale(localeStr);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			TaskManager manager = (TaskManager) Context.get().getBean(TaskManager.class);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			for (Task task : manager.getTasks()) {
				writer.print("<task>");
				writer.print("<name><![CDATA[" + task.getName() + "]]></name>");
				if (task.getScheduling().isEnabled()) {
					writer.print("<eenabled>true</eenabled>");
					writer.print("<enabledIcon>bullet_green</enabledIcon>");
				} else {
					writer.print("<eenabled>false</eenabled>");
					writer.print("<enabledIcon>bullet_red</enabledIcon>");
				}
				writer.print("<status>" + task.getStatus() + "</status>");
				if (task.getScheduling().getMode().equals(TaskTrigger.MODE_CRON))
					writer.print("<scheduling>" + task.getScheduling().getCronExpression() + "</scheduling>");
				else if (task.getScheduling().getMode().equals(TaskTrigger.MODE_SIMPLE))
					writer.print("<scheduling>" + I18N.message("each", locale) + " "
							+ task.getScheduling().getIntervalSeconds() + " "
							+ I18N.message("seconds", locale).toLowerCase() + "</scheduling>");
				if (!task.isIndeterminate())
					writer.print("<completion>" + task.getCompletionPercentage() + "</completion>");
				writer.print("<progress>" + task.getProgress() + "</progress>");
				writer.print("<size>" + task.getSize() + "</size>");
				if (task.getStatus() == Task.STATUS_IDLE)
					writer.print("<runningIcon>idle_task</runningIcon>");
				else
					writer.print("<runningIcon>running_task</runningIcon>");

				if (task.getScheduling().getPreviousFireTime() != null) {
					writer.print("<lastStart>" + df.format(task.getScheduling().getPreviousFireTime()) + "</lastStart>");
				}

				if (task.getScheduling().getNextFireTime() != null) {
					writer.print("<nextStart>" + df.format(task.getScheduling().getNextFireTime()) + "</nextStart>");
				}
				writer.print("<indeterminate>" + "" + task.isIndeterminate() + "</indeterminate>");

				writer.print("</task>");
			}
			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}