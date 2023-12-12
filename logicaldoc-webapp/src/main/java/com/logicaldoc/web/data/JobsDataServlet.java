package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.quartz.JobDetail;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.job.JobManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;

/**
 * This servlet retrieves Job-related data
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class JobsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	protected static Logger logger = LoggerFactory.getLogger(JobsDataServlet.class);

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
				JobManager jobManager = (JobManager) Context.get().getBean(JobManager.class);
				for (String name : jobManager.getGroups()) {
					writer.print("<job>");
					writer.print("<group><![CDATA[" + name + "]]></group>");
					writer.print("</job>");
				}
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
			throws SchedulerException {
		DateFormat df = getDateFormat();

		TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Map<Long, String> tenants = tDao.findAll().stream().collect(Collectors.toMap(t -> t.getId(), Tenant::getName));

		int count = 0;

		JobManager jobManager = (JobManager) Context.get().getBean(JobManager.class);
		for (Trigger trigger : jobManager.getTriggers(group,
				session.getTenantId() == Tenant.DEFAULT_ID ? null : session.getTenantId())) {
			if (count++ >= maxRecords)
				break;

			writer.print("<job>");
			writer.print("<name><![CDATA[" + trigger.getJobKey().getName() + "]]></name>");
			writer.print("<group><![CDATA[" + trigger.getJobKey().getGroup() + "]]></group>");
			writer.print("<trigger><![CDATA[" + trigger.getKey().getName() + "]]></trigger>");
			if (trigger.getJobDataMap() != null && trigger.getJobDataMap().containsKey(JobManager.TENANT_ID)) {
				writer.print("<tenantId>" + trigger.getJobDataMap().getLong(JobManager.TENANT_ID) + "</tenantId>");
				writer.print("<tenant><![CDATA[" + tenants.get(trigger.getJobDataMap().getLong(JobManager.TENANT_ID))
						+ "]]></tenant>");
			}

			JobDetail job = jobManager.getJob(trigger.getJobKey().getName(), trigger.getJobKey().getGroup());
			if (job.getDescription() != null)
				writer.print("<description><![CDATA[" + job.getDescription() + "]]></description>");

			final Date previousFireTime = trigger.getPreviousFireTime();
			Date nextFireTime = trigger.getNextFireTime();
			if (previousFireTime != null) {
				writer.print("<previousFire>" + df.format(previousFireTime) + "</previousFire>");
				nextFireTime=trigger.getFireTimeAfter(previousFireTime);
			}
			
			if (nextFireTime != null )
				writer.print("<nextFire>" + df.format(nextFireTime) + "</nextFire>");
			writer.print("</job>");
		}
	}
}