package com.logicaldoc.web.service;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.DeviceDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.security.PasswordGenerator;

/**
 * Implementation of the <code>LoginService</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LoginServiceImpl extends RemoteServiceServlet implements LoginService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(LoginServiceImpl.class);

	@Override
	public GUIValue changePassword(long userId, String oldPassword, String newPassword) {
		SecurityServiceImpl ser = new SecurityServiceImpl();
		return ser.changePassword(userId, userId, oldPassword, newPassword, false);
	}

	@Override
	public GUIUser getUser(String username) {
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			UserHistoryDAO userHistoryDao = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
			TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);

			User user = pickUser(username);

			// Get just a few informations needed by the login
			GUIUser usr = new GUIUser();
			usr.setId(user.getId());
			usr.setEnabled(user.getEnabled() == 1);
			usr.setUsername(user.getUsername());
			usr.setTenant(SecurityServiceImpl.getTenant(user.getTenantId()));
			usr.setPasswordExpires(user.getPasswordExpires() == 1);
			usr.setPasswordExpired(userDao.isPasswordExpired(username));
			usr.setEmail(user.getEmail());
			usr.setEmail2(user.getEmail2());
			usr.setName(user.getName());
			usr.setFirstName(user.getFirstName());
			usr.setSecondFactor(user.getSecondFactor());

			Tenant tenant = tenantDao.findById(user.getTenantId());

			ContextProperties config = Context.get().getProperties();
			usr.setPasswordMinLenght(config.getInt(tenant.getName() + ".password.size", 6));

			// Retrieve the reason for the last login failure
			List<UserHistory> failures = userHistoryDao.findByUserIdAndEvent(user.getId(),
					UserEvent.LOGIN_FAILED.toString());
			if (failures != null && !failures.isEmpty())
				usr.setLastLoginFailureReason(failures.get(0).getComment());

			return usr;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}

	@Override
	public void resetPassword(String username, String emailAddress, String productName) throws ServerException {
		User user = pickUser(username);
		if (!user.getEmail().trim().equals(emailAddress.trim()))
			throw new ServerException(String.format("Email %s is wrong", emailAddress));

		try {
			// Prepare a new password ticket
			String ticketid = (UUID.randomUUID().toString());
			Ticket ticket = new Ticket();
			ticket.setTicketId(ticketid);
			ticket.setDocId(0L);
			ticket.setUserId(user.getId());
			ticket.setTenantId(user.getTenantId());
			ticket.setType(Ticket.PSW_RECOVERY);
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.MINUTE, +15);
			ticket.setExpired(cal.getTime());

			// Store the ticket
			TicketDAO ticketDao = (TicketDAO) Context.get().getBean(TicketDAO.class);
			ticketDao.store(ticket);

			// Try to clean the DB from old tickets
			ticketDao.deleteExpired();

			Locale locale = user.getLocale();

			EMail email = new EMail();
			email.setHistoricyze(false);
			email.setHtml(1);
			email.setTenantId(user.getTenantId());
			Recipient recipient = new Recipient();
			recipient.setAddress(user.getEmail());
			recipient.setRead(1);
			email.addRecipient(recipient);
			email.setFolder("outbox");
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
			Map<String, Object> dictionary = new HashMap<>();
			dictionary.put("product", productName);
			dictionary.put("url", address);
			dictionary.put("user", user);
			dictionary.put(Automation.LOCALE, locale);

			EMailSender sender = new EMailSender(user.getTenantId());
			sender.send(email, "psw.rec2", dictionary);
		} catch (Exception e) {
			throw new ServerException(e.getMessage());
		}
	}

	private User pickUser(String username) throws ServerException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user;
		try {
			user = userDao.getUser(username);
		} catch (PersistenceException e) {
			throw new ServerException(String.format("Error in the data layer for user %s", username));
		}
		if (user == null)
			throw new ServerException(String.format("User %s not found", username));
		return user;
	}

	@Override
	public boolean isSecretKeyRequired(String username, String deviceId) throws ServerException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user;
		try {
			user = pickUser(username);
		} catch (ServerException e) {
			return false;
		}

		userDao.initialize(user);

		if (StringUtils.isEmpty(user.getSecondFactor()))
			return false;

		TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenant = Tenant.SYSTEM_NAME;
		try {
			tenant = tDao.getTenantName(user.getTenantId());
		} catch (PersistenceException e) {
			log.warn("Cannot retrieve tenant name of user {}", user.getUsername());
		}

		ContextProperties config = Context.get().getProperties();
		if (!config.getBoolean(tenant + ".2fa.enabled", false)
				|| !config.getBoolean(tenant + ".2fa." + user.getSecondFactor().toLowerCase() + ".enabled", false))
			return false;

		if (config.getBoolean(tenant + ".2fa.allowtrusted", true)) {
			HttpServletRequest request = getThreadLocalRequest();
			request.setAttribute(Device.PARAM_DEVICE, deviceId);

			DeviceDAO deviceDao = (DeviceDAO) Context.get().getBean(DeviceDAO.class);
			try {
				return !deviceDao.isTrustedDevice(user.getUsername(), request);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
				return false;
			}
		} else
			return true;
	}

	@Override
	public String generatePassword(String username) {
		User user;
		try {
			user = pickUser(username);
		} catch (ServerException e) {
			log.warn(e.getMessage(), e);
			return "";
		}

		TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenant = Tenant.DEFAULT_NAME;
		try {
			tenant = tDao.getTenantName(user.getTenantId());
		} catch (PersistenceException e) {
			log.warn("Cannot retrieve tenant name of user {}", user.getUsername());
		}

		// Generate an initial password(that must be changed)
		ContextProperties config = Context.get().getProperties();
		return PasswordGenerator.generate(config.getInt(tenant + ".password.size", 8),
				config.getInt(tenant + ".password.uppercase", 2), config.getInt(tenant + ".password.lowercase", 2),
				config.getInt(tenant + ".password.digit", 1), config.getInt(tenant + ".password.special", 1),
				config.getInt(tenant + ".password.sequence", 4), config.getInt(tenant + ".password.occurrence", 3));
	}
}