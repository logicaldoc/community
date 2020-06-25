package com.logicaldoc.web.service;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Implementation of the <code>LoginService</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LoginServiceImpl extends RemoteServiceServlet implements LoginService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(InfoServiceImpl.class);

	@Override
	public int changePassword(long userId, String oldPassword, String newPassword) {
		SecurityServiceImpl ser = new SecurityServiceImpl();
		return ser.changePassword(userId, userId, oldPassword, newPassword, false);
	}

	@Override
	public GUIUser getUser(String username) {
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);

			User user = userDao.findByUsername(username);
			if (user == null)
				return null;

			// Get just a few informations needed by the login
			GUIUser usr = new GUIUser();
			usr.setId(user.getId());
			usr.setTenant(SecurityServiceImpl.getTenant(user.getTenantId()));
			usr.setPasswordExpires(user.getPasswordExpires() == 1);
			usr.setPasswordExpired(user.getPasswordExpired() == 1);

			Tenant tenant = tenantDao.findById(user.getTenantId());

			ContextProperties config = Context.get().getProperties();
			usr.setPasswordMinLenght(Integer.parseInt(config.getProperty(tenant.getName() + ".password.size")));

			return usr;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}

	@Override
	public void resetPassword(String username, String emailAddress, String productName) throws ServerException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDao.findByUsername(username);

		if (user == null)
			throw new ServerException(String.format("User %s not found", username));
		else if (!user.getEmail().trim().equals(emailAddress.trim()))
			throw new ServerException(String.format("Email %s is wrong", emailAddress));

		try {
			EMail email;
			email = new EMail();
			email.setHtml(1);
			email.setTenantId(user.getTenantId());
			Recipient recipient = new Recipient();
			recipient.setAddress(user.getEmail());
			recipient.setRead(1);
			email.addRecipient(recipient);
			email.setFolder("outbox");

			// Prepare a new download ticket
			String ticketid = (UUID.randomUUID().toString());
			Ticket ticket = new Ticket();
			ticket.setTicketId(ticketid);
			ticket.setDocId(0L);
			ticket.setUserId(user.getId());
			ticket.setTenantId(user.getTenantId());
			ticket.setType(Ticket.PSW_RECOVERY);
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.MINUTE, +5);
			ticket.setExpired(cal.getTime());

			// Store the ticket
			TicketDAO ticketDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			ticketDao.store(ticket);

			// Try to clean the DB from old tickets
			ticketDao.deleteExpired();

			Locale locale = user.getLocale();

			email.setLocale(locale);
			email.setSentDate(new Date());
			email.setUsername(user.getUsername());

			HttpServletRequest request = this.getThreadLocalRequest();
			String urlPrefix = request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort()
					+ request.getContextPath();
			String address = urlPrefix + "/pswrecovery?ticketId=" + ticketid + "&userId=" + user.getId();

			/*
			 * Prepare the template
			 */
			Map<String, Object> dictionary = new HashMap<String, Object>();
			dictionary.put("product", productName);
			dictionary.put("url", address);
			dictionary.put("user", user);
			dictionary.put(Automation.LOCALE, locale);

			EMailSender sender = new EMailSender(user.getTenantId());
			sender.send(email, "psw.rec2", dictionary);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new ServerException(e.getMessage());
		}
	}
}