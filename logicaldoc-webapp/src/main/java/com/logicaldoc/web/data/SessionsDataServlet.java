package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.SessionDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet is responsible for sessions data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SessionsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		// Headers required by Internet Explorer
		response.setHeader("Pragma", "public");
		response.setHeader("Cache-Control", "must-revalidate, post-check=0,pre-check=0");
		response.setHeader("Expires", "0");

		if (request.getParameter("kill") != null) {
			// Kill a specific session
			SessionManager.get().kill(request.getParameter("kill"));
			System.out.println(String.format("Killed session %s", request.getParameter("kill")));
			PrintWriter writer = response.getWriter();
			writer.println("ok");
		} else {
			String node = request.getParameter("node");
			Integer status = request.getParameter("status") != null ? Integer.parseInt(request.getParameter("status"))
					: null;

			// Just listing the sessions
			SessionDAO sessionDao = (SessionDAO) Context.get().getBean(SessionDAO.class);
			List<Session> sessions = sessionDao.findByNode(node);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			boolean csvFormat = "true".equals(request.getParameter("csv"));

			/*
			 * Try to check tenant filter for those users trying to list the
			 * sessions from another tenant
			 */
			String tenant = null;

			Session currentSession = null;
			if (request.getServletPath().contains("data")) {
				currentSession = ServletUtil.validateSession(request);
				if (!Tenant.DEFAULT_NAME.equals(currentSession.getTenantName()))
					tenant = currentSession.getTenantName();
			}

			User currentUser = null;
			if (currentSession != null)
				currentUser = currentSession.getUser();
			else
				try {
					currentUser = ServletUtil.getSessionUser(request);
				} catch (Throwable t) {
					// Nothing to do
				}

			/*
			 * The current user must be enabled to see the sessions.
			 */
			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			boolean showSid = currentUser == null
					|| (currentUser != null && mDao.isReadEnable(Menu.ADMIN_SESSIONS, currentUser.getId()));

			PrintWriter writer = response.getWriter();
			if (!csvFormat)
				writer.print("<list>");

			for (Session sess : sessions) {
				if (tenant != null && !tenant.equals(sess.getTenantName()))
					continue;
				if (status != null && status != sess.getStatus())
					continue;

				if (csvFormat) {
					writer.print(showSid ? sess.getSid() : "--");
					writer.print(",");
					if (showSid)
						if (SessionManager.get().getStatus(sess.getSid()) == Session.STATUS_OPEN)
							writer.print(I18N.message("opened", locale));
						else if (SessionManager.get().getStatus(sess.getSid()) == Session.STATUS_CLOSED)
							writer.print(I18N.message("closed", locale));
						else if (SessionManager.get().getStatus(sess.getSid()) == Session.STATUS_EXPIRED)
							writer.print(I18N.message("expired", locale));
					writer.print(",");
					if (showSid)
						writer.print(sess.getUsername());
					writer.print(",");
					if (showSid)
						writer.print(sess.getClient() != null ? sess.getClient() : "");
					writer.print(",");
					writer.print(sess.getTenantName());
					writer.print(",");
					writer.print(df.format((Date) sess.getCreation()));
					writer.print(",");
					if (showSid)
						if (SessionManager.get().get(sess.getSid()) != null)
							writer.print(SessionManager.get().get(sess.getSid()).getLastRenew());
						else
							writer.print(df.format((Date) sess.getLastRenew()));
					writer.print(",");
					if (showSid)
						writer.print(sess.getNode());
					writer.print("\n");
				} else {
					writer.print("<session>");
					writer.print("<sid><![CDATA[" + (showSid ? sess.getSid() : "--") + "]]></sid>");
					writer.print("<status>" + (showSid ? sess.getStatus() : "") + "</status>");
					if (showSid) {
						if (SessionManager.get().getStatus(sess.getSid()) == Session.STATUS_OPEN)
							writer.print("<statusLabel>" + I18N.message("opened", locale) + "</statusLabel>");
						else if (SessionManager.get().getStatus(sess.getSid()) == Session.STATUS_CLOSED)
							writer.print("<statusLabel>" + I18N.message("closed", locale) + "</statusLabel>");
						else if (SessionManager.get().getStatus(sess.getSid()) == Session.STATUS_EXPIRED)
							writer.print("<statusLabel>" + I18N.message("expired", locale) + "</statusLabel>");
					} else {
						writer.print("<statusLabel></statusLabel>");
					}
					writer.print("<username><![CDATA[" + (showSid ? sess.getUsername() : "") + "]]></username>");
					writer.print("<node><![CDATA[" + (showSid ? sess.getNode() : "") + "]]></node>");
					writer.print("<client><![CDATA["
							+ (showSid ? (sess.getClient() != null ? sess.getClient() : "") : "") + "]]></client>");
					writer.print("<tenant><![CDATA[" + sess.getTenantName() + "]]></tenant>");
					writer.print("<created>" + df.format((Date) sess.getCreation()) + "</created>");
					if (SessionManager.get().get(sess.getSid()) != null)
						writer.print("<renew>" + df.format(SessionManager.get().get(sess.getSid()).getLastRenew())
								+ "</renew>");
					else
						writer.print("<renew>" + df.format((Date) sess.getLastRenew()) + "</renew>");
					writer.print("</session>");
				}
			}

			if (!csvFormat)
				writer.print("</list>");
		}
	}
}