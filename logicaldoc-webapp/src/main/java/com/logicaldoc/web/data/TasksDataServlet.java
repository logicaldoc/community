package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskManager;
import com.logicaldoc.core.task.TaskTrigger;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.time.TimeDiff;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for tasks data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TasksDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		TaskManager manager = Context.get(TaskManager.class);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		for (Task task : manager.getTasks()) {
			printTask(writer, locale, task);
		}
		writer.write("</list>");
	}

	private void printTask(PrintWriter writer, Locale locale, Task task) {
		DateFormat df = getDateFormat();
		writer.print("<task>");
		writer.print(String.format("<name><![CDATA[%s]]></name>", task.getName()));
		writer.print(
				String.format("<label><![CDATA[%s]]></label>", I18N.message("task.name." + task.getName(), locale)));

		writer.print(String.format("<description><![CDATA[%s]]></description>",
				I18N.message("task.description." + task.getName(), locale)));

		if (task.getScheduling().isEnabled()) {
			writer.print("<eenabled>true</eenabled>");
			writer.print("<enabledIcon>bullet_green</enabledIcon>");
		} else {
			writer.print("<eenabled>false</eenabled>");
			writer.print("<enabledIcon>bullet_red</enabledIcon>");
		}

		writer.print(String.format("<status>%d</status>", task.getStatus()));
		writer.print(String.format("<scheduling>%s</scheduling>", switch (task.getScheduling().getMode()) {
			case TaskTrigger.MODE_CRON -> task.getScheduling().getCronExpression();
			default -> String.format("%s %d %s", I18N.message("each", locale),
					task.getScheduling().getIntervalSeconds(), I18N.message("seconds", locale).toLowerCase());
		}));

		if (task.getScheduling().getMode().equals(TaskTrigger.MODE_CRON))
			writer.print("<scheduling>" + task.getScheduling().getCronExpression() + "</scheduling>");
		else if (task.getScheduling().getMode().equals(TaskTrigger.MODE_SIMPLE))
			writer.print("<scheduling>" + I18N.message("each", locale) + " " + task.getScheduling().getIntervalSeconds()
					+ " " + I18N.message("seconds", locale).toLowerCase() + "</scheduling>");
		if (!task.isIndeterminate())
			writer.print(String.format("<completion>%d</completion>", task.getCompletionPercentage()));
		writer.print(String.format("<progress>%d</progress>", task.getProgress()));
		writer.print(String.format("<size>%d</size>", task.getSize()));
		writer.print(String.format("<runningIcon>%s</runningIcon>", switch (task.getStatus()) {
			case Task.STATUS_IDLE -> "idle_task";
			default -> "running_task";
		}));

		Date previousFireTime = task.getScheduling().getPreviousFireTime();
		if (previousFireTime != null)
			writer.print(String.format("<lastStart>%s</lastStart>", df.format(previousFireTime)));

		Date nextFireTime = task.getScheduling().getNextFireTime();
		if (nextFireTime != null)
			writer.print(String.format("<nextStart>%s</nextStart>", df.format(task.getScheduling().getNextFireTime())));

		writer.print(String.format("<indeterminate>%b</indeterminate>", task.isIndeterminate()));
		writer.print(String.format("<duration>%s</duration>",
				TimeDiff.printDuration(task.getScheduling().getLastDuration())));

		writer.print("</task>");
	}
}