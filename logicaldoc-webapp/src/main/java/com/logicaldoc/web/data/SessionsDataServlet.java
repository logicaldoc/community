package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.text.DateFormat;
import java.util.List;
import java.util.Locale;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionDAO;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet is responsible for sessions data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SessionsDataServlet extends AbstractDataServlet {

	private static final Logger log = LoggerFactory.getLogger(SessionsDataServlet.class);

	private static final String CLOSE_STATUS_LABEL = "</statusLabel>";

	private static final String STATUS_LABEL = "<statusLabel>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		if (request.getParameter("kill") != null) {
			// Kill a specific session
			SessionManager.get().kill(request.getParameter("kill"));
			if (log.isDebugEnabled())
				log.debug("Killed session {}", request.getParameter("kill"));
			PrintWriter writer = response.getWriter();
			writer.println("ok");
		} else {
			String node = request.getParameter("node");
			Integer status = request.getParameter("status") != null ? Integer.parseInt(request.getParameter("status"))
					: null;

			// Just listing the sessions
			SessionDAO sessionDao = Context.get(SessionDAO.class);
			List<Session> sessions = sessionDao.findByNode(node);

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

			User currentUser = getCurrentUser(request, currentSession);

			/*
			 * The current user must be enabled to see the sessions.
			 */
			MenuDAO mDao = Context.get(MenuDAO.class);
			boolean showSid = currentUser == null || mDao.isReadEnable(Menu.ADMIN_SESSIONS, currentUser.getId());

			PrintWriter writer = response.getWriter();
			if (!csvFormat)
				writer.print("<list>");

			printSessions(locale, status, sessions, csvFormat, tenant, showSid, writer);
		}
	}

	private void printSessions(Locale locale, Integer status, List<Session> sessions, boolean csvFormat, String tenant,
			boolean showSid, PrintWriter writer) {

		for (Session session : sessions) {
			if ((tenant != null && !tenant.equals(session.getTenantName()))
					|| (status != null && status != session.getStatus()))
				continue;

			if (csvFormat) {
				printSessionCsv(writer, session, locale, showSid);
			} else {
				printSessionXml(writer, session, locale, showSid);
			}
		}

		if (!csvFormat)
			writer.print("</list>");
	}

	private void printSessionCsv(PrintWriter writer, Session session, Locale locale, boolean showSid) {
		DateFormat df = getDateFormat();

		writer.print(showSid ? session.getSid() : "--");
		writer.print(",");

		if (showSid)
			printSessionStatusCsv(session, locale, writer);

		writer.print(",");
		if (showSid)
			writer.print(session.getUsername());
		writer.print(",");
		if (showSid)
			writer.print(session.getClient() != null ? session.getClient() : "");
		writer.print(",");
		writer.print(session.getTenantName());
		writer.print(",");
		writer.print(df.format(session.getCreation()));
		writer.print(",");

		if (showSid) {
			if (SessionManager.get().get(session.getSid()) != null)
				writer.print(SessionManager.get().get(session.getSid()).getLastRenew());
			else
				writer.print(df.format(session.getLastRenew()));
		}

		writer.print(",");
		writer.print(StringUtils.defaultString(session.getKeyLabel()));
		writer.print(",");
		if (showSid)
			writer.print(session.getNode());
		writer.print("\n");
	}

	private void printSessionStatusCsv(Session session, Locale locale, PrintWriter writer) {
		int status = SessionManager.get().getStatus(session.getSid());
		if (status < 0)
			status = session.getStatus();

		if (status == Session.STATUS_OPEN)
			writer.print(I18N.message("opened", locale));
		else if (status == Session.STATUS_CLOSED)
			writer.print(I18N.message("closed", locale));
		else if (status == Session.STATUS_EXPIRED)
			writer.print(I18N.message("expired", locale));
	}

	private void printSessionXml(PrintWriter writer, Session session, Locale locale, boolean showSid) {
		DateFormat df = getDateFormat();

		writer.print("<session>");
		writer.print("<sid><![CDATA[" + (showSid ? session.getSid() : "--") + "]]></sid>");
		writer.print("<key><![CDATA[" + StringUtils.defaultString(session.getKeyLabel()) + "]]></key>");
		printSessionStatusXml(session, locale, showSid, writer);

		writer.print("<username><![CDATA[" + (showSid ? session.getUsername() : "") + "]]></username>");
		writer.print("<node><![CDATA[" + (showSid ? session.getNode() : "") + "]]></node>");

		final Serializable client = session.getClient() != null ? session.getClient() : "";
		writer.print("<client><![CDATA[" + (showSid ? client : "") + "]]></client>");
		writer.print("<tenant><![CDATA[" + session.getTenantName() + "]]></tenant>");
		writer.print("<created>" + df.format(session.getCreation()) + "</created>");
		if (session.getFinished() != null)
			writer.print("<finished>" + df.format(session.getCreation()) + "</finished>");
		writer.print("<duration>" + TimeDiff.printDuration(session.getDuration()) + "</duration>");
		if (SessionManager.get().get(session.getSid()) != null)
			writer.print("<renew>" + df.format(SessionManager.get().get(session.getSid()).getLastRenew()) + "</renew>");
		else
			writer.print("<renew>" + df.format(session.getLastRenew()) + "</renew>");
		writer.print("</session>");
	}

	private void printSessionStatusXml(Session session, Locale locale, boolean showSid, PrintWriter writer) {
		writer.print("<status>" + (showSid ? session.getStatus() : "") + "</status>");
		if (showSid) {
			int status = SessionManager.get().getStatus(session.getSid());
			if (status < 0)
				status = session.getStatus();

			if (status == Session.STATUS_OPEN)
				writer.print(STATUS_LABEL + I18N.message("opened", locale) + CLOSE_STATUS_LABEL);
			else if (status == Session.STATUS_CLOSED)
				writer.print(STATUS_LABEL + I18N.message("closed", locale) + CLOSE_STATUS_LABEL);
			else if (status == Session.STATUS_EXPIRED)
				writer.print(STATUS_LABEL + I18N.message("expired", locale) + CLOSE_STATUS_LABEL);
		} else {
			writer.print("<statusLabel></statusLabel>");
		}
	}

	private User getCurrentUser(HttpServletRequest request, Session currentSession) {
		User currentUser = null;
		if (currentSession != null)
			currentUser = currentSession.getUser();
		else
			try {
				currentUser = ServletUtil.getSessionUser(request);
			} catch (Exception t) {
				// Nothing to do
			}
		return currentUser;
	}

	@Override
	protected boolean isSessionRequired() {
		return false;
	}
}