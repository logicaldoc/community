package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.quartz.JobDetail;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.job.JobManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet retrieves Job-related data
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class JobsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger logger = LoggerFactory.getLogger(JobsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		int maxRecords = max != null ? max : 100;

		String group = request.getParameter("group");

		boolean groupsonly = request.getParameter("groupsonly") != null;

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Iterate over records composing the response XML document
		 */
		if (groupsonly) {
			try {
				JobManager jobManager = Context.get(JobManager.class);
				for (String name : jobManager.getGroups())
					writer.print(String.format("<job><group><![CDATA[%s]]></group></job>", name));
			} catch (SchedulerException e) {
				logger.warn(e.getMessage(), e);
			}
		} else {
			try {
				writeJobs(writer, session, maxRecords, group);
			} catch (SchedulerException e) {
				logger.warn(e.getMessage(), e);
			}
		}
		writer.write("</list>");
	}

	private void writeJobs(PrintWriter writer, Session session, int maxRecords, String group)
			throws SchedulerException, PersistenceException {
		DateFormat df = getDateFormat();

		Map<Long, String> tenants = TenantDAO.get().findAll().stream().collect(Collectors.toMap(t -> t.getId(), Tenant::getName));

		int count = 0;

		JobManager jobManager = Context.get(JobManager.class);
		for (Trigger trigger : jobManager.getTriggers(group,
				session.getTenantId() == Tenant.DEFAULT_ID ? null : session.getTenantId())) {
			if (count++ >= maxRecords)
				break;

			writer.print("<job>");
			writer.print(String.format("<name><![CDATA[%s]]></name>", trigger.getJobKey().getName()));
			writer.print(String.format("<group><![CDATA[%s]]></group>", trigger.getJobKey().getGroup()));
			writer.print(String.format("<trigger><![CDATA[%s]]></trigger>", trigger.getKey().getName()));
			if (trigger.getJobDataMap() != null && trigger.getJobDataMap().containsKey(JobManager.TENANT_ID)) {
				writer.print(String.format("<tenantId>%d</tenantId>", trigger.getJobDataMap().getLong(JobManager.TENANT_ID)));
				writer.print(String.format("<tenant><![CDATA[%s]]></tenant>", tenants.get(trigger.getJobDataMap().getLong(JobManager.TENANT_ID))));
			}

			JobDetail job = jobManager.getJob(trigger.getJobKey().getName(), trigger.getJobKey().getGroup());
			if (job.getDescription() != null)
				writer.print(String.format("<description><![CDATA[%s]]></description>", job.getDescription()));

			final Date previousFireTime = trigger.getPreviousFireTime();
			Date nextFireTime = trigger.getNextFireTime();
			if (previousFireTime != null) {
				writer.print(String.format("<previousFire>%s</previousFire>", df.format(previousFireTime)));
				nextFireTime=trigger.getFireTimeAfter(previousFireTime);
			}
			
			if (nextFireTime != null )
				writer.print(String.format("<nextFire>%s</nextFire>", df.format(nextFireTime)));
			writer.print("</job>");
		}
	}
}