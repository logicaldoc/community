package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.SessionDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for sessions data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SessionsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SessionsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
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
				Integer status = request.getParameter("status") != null
						? Integer.parseInt(request.getParameter("status"))
						: null;

				// Just listing the sessions
				SessionDAO sessionDao = (SessionDAO) Context.get().getBean(SessionDAO.class);
				List<Session> sessions = sessionDao.findByNode(node);
				DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
				df.setTimeZone(TimeZone.getTimeZone("UTC"));

				String locale = request.getParameter("locale");

				boolean csvFormat = "true".equals(request.getParameter("csv"));

				/*
				 * Try to check tenant filter for those users trying to list the
				 * sessions from another tenant
				 */
				String tenant = null;

				Session currentSession = null;
				if (request.getServletPath().contains("data")) {
					currentSession = ServiceUtil.validateSession(request);
					if (!Tenant.DEFAULT_NAME.equals(currentSession.getTenantName()))
						tenant = currentSession.getTenantName();
				}

				User currentUser = null;
				if (currentSession != null)
					currentUser = currentSession.getUser();
				else
					try {
						currentUser = ServiceUtil.getSessionUser(request);
					} catch (Throwable t) {

					}

				/*
				 * The current user must be enabled to see the sessions.
				 */
				MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
				boolean showSid = currentUser==null || (currentUser != null && mDao.isReadEnable(Menu.ADMIN_SESSIONS, currentUser.getId()));

				PrintWriter writer = response.getWriter();
				if (!csvFormat)
					writer.print("<list>");

				for (Session session : sessions) {
					if (tenant != null && !tenant.equals(session.getTenantName()))
						continue;
					if (status != null && status != session.getStatus())
						continue;

					if (csvFormat) {
						writer.print(showSid ? session.getSid() : "--");
						writer.print(",");
						if (showSid)
							if (SessionManager.get().getStatus(session.getSid()) == Session.STATUS_OPEN)
								writer.print(I18N.message("opened", locale));
							else if (SessionManager.get().getStatus(session.getSid()) == Session.STATUS_CLOSED)
								writer.print(I18N.message("closed", locale));
							else if (SessionManager.get().getStatus(session.getSid()) == Session.STATUS_EXPIRED)
								writer.print(I18N.message("expired", locale));
						writer.print(",");
						if (showSid)
							writer.print(session.getUsername());
						writer.print(",");
						if (showSid)
							writer.print(session.getClient() != null ? session.getClient() : "");
						writer.print(",");
						writer.print(session.getTenantName());
						writer.print(",");
						writer.print(df.format((Date) session.getCreation()));
						writer.print(",");
						if (showSid)
							if (SessionManager.get().get(session.getSid()) != null)
								writer.print(SessionManager.get().get(session.getSid()).getLastRenew());
							else
								writer.print(df.format((Date) session.getLastRenew()));
						writer.print(",");
						if (showSid)
							writer.print(session.getNode());
						writer.print("\n");
					} else {
						writer.print("<session>");
						writer.print("<sid><![CDATA[" + (showSid ? session.getSid() : "--") + "]]></sid>");
						writer.print("<status>" + (showSid ? session.getStatus() : "") + "</status>");
						if (showSid) {
							if (SessionManager.get().getStatus(session.getSid()) == Session.STATUS_OPEN)
								writer.print("<statusLabel>" + I18N.message("opened", locale) + "</statusLabel>");
							else if (SessionManager.get().getStatus(session.getSid()) == Session.STATUS_CLOSED)
								writer.print("<statusLabel>" + I18N.message("closed", locale) + "</statusLabel>");
							else if (SessionManager.get().getStatus(session.getSid()) == Session.STATUS_EXPIRED)
								writer.print("<statusLabel>" + I18N.message("expired", locale) + "</statusLabel>");
						} else {
							writer.print("<statusLabel></statusLabel>");
						}
						writer.print("<username><![CDATA[" + (showSid ? session.getUsername() : "") + "]]></username>");
						writer.print("<node><![CDATA[" + (showSid ? session.getNode() : "") + "]]></node>");
						writer.print("<client><![CDATA["
								+ (showSid ? (session.getClient() != null ? session.getClient() : "") : "")
								+ "]]></client>");
						writer.print("<tenant><![CDATA[" + session.getTenantName() + "]]></tenant>");
						writer.print("<created>" + df.format((Date) session.getCreation()) + "</created>");
						if (SessionManager.get().get(session.getSid()) != null)
							writer.print(
									"<renew>" + df.format(SessionManager.get().get(session.getSid()).getLastRenew())
											+ "</renew>");
						else
							writer.print("<renew>" + df.format((Date) session.getLastRenew()) + "</renew>");
						writer.print("</session>");
					}
				}

				if (!csvFormat)
					writer.print("</list>");
			}
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