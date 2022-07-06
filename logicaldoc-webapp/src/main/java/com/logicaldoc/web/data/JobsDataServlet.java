package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Map;
import java.util.TimeZone;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.quartz.JobDetail;
import org.quartz.Trigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.job.JobManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet retrieves Job-related data
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class JobsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(JobsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			Integer max = request.getParameter("max") != null ? Integer.parseInt(request.getParameter("max")) : null;

			String group = request.getParameter("group");

			boolean groupsonly = request.getParameter("groupsonly") != null;

			JobManager jobManager = (JobManager) Context.get().getBean(JobManager.class);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			/*
			 * Iterate over records composing the response XML document
			 */
			if (groupsonly) {
				for (String name : jobManager.getGroups()) {
					writer.print("<job>");
					writer.print("<group><![CDATA[" + name + "]]></group>");
					writer.print("</job>");
				}
			} else {
				TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
				Map<Long, String> tenants = tDao.findAll().stream()
						.collect(Collectors.toMap(t -> t.getId(), t -> t.getName()));

				int count = 0;
				for (Trigger trigger : jobManager.getTriggers(group,
						session.getTenantId() == Tenant.DEFAULT_ID ? null : session.getTenantId())) {
					if (max != null && count++ >= max)
						break;

					writer.print("<job>");
					writer.print("<name><![CDATA[" + trigger.getJobKey().getName() + "]]></name>");
					writer.print("<group><![CDATA[" + trigger.getJobKey().getGroup() + "]]></group>");
					writer.print("<trigger><![CDATA[" + trigger.getKey().getName() + "]]></trigger>");
					if (trigger.getJobDataMap() != null && trigger.getJobDataMap().containsKey(JobManager.TENANT_ID)) {
						writer.print(
								"<tenantId>" + trigger.getJobDataMap().getLong(JobManager.TENANT_ID) + "</tenantId>");
						writer.print("<tenant><![CDATA["
								+ tenants.get(trigger.getJobDataMap().getLong(JobManager.TENANT_ID)) + "]]></tenant>");
					}

					JobDetail job = jobManager.getJob(trigger.getJobKey().getName(), trigger.getJobKey().getGroup());
					if (job.getDescription() != null)
						writer.print("<description><![CDATA[" + job.getDescription() + "]]></description>");

					if (trigger.getPreviousFireTime() != null)
						writer.print("<previousFire>" + df.format(trigger.getPreviousFireTime()) + "</previousFire>");
					if (trigger.getNextFireTime() != null)
						writer.print("<nextFire>" + df.format(trigger.getNextFireTime()) + "</nextFire>");
					writer.print("</job>");
				}
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