package com.logicaldoc.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.mail.MessagingException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.security.PasswordGenerator;

/**
 * This class allows the user to recover a password.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class PswRecovery extends HttpServlet {

	private static final String TICKET_ID = "ticketId";

	private static final long serialVersionUID = 9088160958327454062L;

	protected static Logger log = LoggerFactory.getLogger(PswRecovery.class);

	/**
	 * Constructor of the object.
	 */
	public PswRecovery() {
		super();
	}

	/**
	 * The doGet method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to get.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 * @throws ServletException if an error occurred
	 * @throws IOException if an error occurred
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			HttpSession session = request.getSession();
			String ticketId = request.getParameter(TICKET_ID);
			String userId = request.getParameter("userId");
			String tenant = request.getParameter("tenant");
			if (tenant == null)
				tenant = Tenant.DEFAULT_NAME;

			if (StringUtils.isEmpty(ticketId)) {
				ticketId = (String) request.getAttribute(TICKET_ID);
			}

			if (StringUtils.isEmpty(ticketId)) {
				ticketId = (String) session.getAttribute(TICKET_ID);
			}

			log.debug("Recover password for ticket with ticketId={}", ticketId);

			TicketDAO ticketDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			Ticket ticket = ticketDao.findByTicketId(ticketId);

			if ((ticket != null) && ticket.getType() == Ticket.PSW_RECOVERY) {

				if (ticket.isTicketExpired()) {
					response.getWriter().println("Request has exprired");
					return;
				}

				UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
				User user = userDao.findById(Long.parseLong(userId));

				sendEmail(request, response, tenant, ticket, user);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);

			writeResponse(response, "Request not correctly processed");
		}
	}

	private void writeResponse(HttpServletResponse response, String message) {
		try {
			response.getWriter().println(message);
		} catch (IOException e) {
			// Ignore
		}
	}

	private void sendEmail(HttpServletRequest request, HttpServletResponse response, String tenant, Ticket ticket,
			User user) throws IOException, PersistenceException, MessagingException, NoSuchAlgorithmException {

		EMail email = new EMail();
		email.setHtml(1);
		Recipient recipient = new Recipient();
		recipient.setRead(1);
		recipient.setAddress(user.getEmail());
		email.addRecipient(recipient);
		email.setFolder("outbox");

		// Generate a new password
		ContextProperties config = Context.get().getProperties();
		String password = PasswordGenerator.generate(config.getInt(tenant + ".password.size", 8),
				config.getInt(tenant + ".password.uppercase", 2), config.getInt(tenant + ".password.lowercase", 2),
				config.getInt(tenant + ".password.digit", 1), config.getInt(tenant + ".password.special", 1),
				config.getInt(tenant + ".password.sequence", 4), config.getInt(tenant + ".password.occurrence", 3));
		user.setDecodedPassword(password);
		user.setPasswordChanged(new Date());
		user.setPasswordExpired(1);

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.store(user);

		Locale locale = user.getLocale();

		email.setSentDate(new Date());
		email.setUsername(user.getUsername());
		email.setLocale(locale);
		email.setHtml(1);

		/*
		 * Prepare the template
		 */
		Map<String, Object> dictionary = new HashMap<>();
		String address = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort()
				+ request.getContextPath();
		dictionary.put("url", address);
		dictionary.put("user", user);
		dictionary.put("password", password);
		dictionary.put(Automation.LOCALE, locale);

		EMailSender sender = new EMailSender(tenant);
		sender.send(email, "psw.rec1", dictionary);

		response.getWriter().println(String.format("A message was sent to %s", user.getEmail()));

		ticket.setCount(ticket.getCount() + 1);

		TicketDAO ticketDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
		ticketDao.store(ticket);
	}

	/**
	 * The doPost method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to
	 * post.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 * @throws ServletException if an error occurred
	 * @throws IOException if an error occurred
	 */
	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			response.setContentType("text/html");

			PrintWriter out = response.getWriter();
			out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
			out.println("<HTML>");
			out.println("  <HEAD><TITLE>Download Ticket Action</TITLE></HEAD>");
			out.println("  <BODY>");
			out.print("    This is ");
			out.print(this.getClass());
			out.println(", using the POST method");
			out.println("  </BODY>");
			out.println("</HTML>");
			out.flush();
			out.close();
		} catch (Exception e) {
			// Nothing to do
		}
	}
}
