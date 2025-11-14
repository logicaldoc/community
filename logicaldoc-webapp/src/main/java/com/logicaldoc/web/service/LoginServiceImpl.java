package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.jakarta.RemoteServiceServlet;
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
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.login.client.services.LoginService;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.security.PasswordGenerator;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Implementation of the <code>LoginService</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LoginServiceImpl extends RemoteServiceServlet implements LoginService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(LoginServiceImpl.class);

	@Override
	public GUIValue changePassword(long userId, String oldPassword, String newPassword) {
		SecurityServiceImpl ser = new SecurityServiceImpl();
		return ser.changePassword(userId, userId, oldPassword, newPassword, false);
	}

	@Override
	public GUIUser getUser(String username) {
		try {
			User user = pickUser(username);

			// Get just a few informations needed by the login
			GUIUser usr = new GUIUser();
			usr.setId(user.getId());
			usr.setEnabled(user.isEnabled());
			usr.setUsername(user.getUsername());
			usr.setTenant(SecurityServiceImpl.getTenant(user.getTenantId()));
			usr.setPasswordExpires(user.isPasswordExpires());
			usr.setPasswordExpired(UserDAO.get().isPasswordExpired(username));
			usr.setEmail(user.getEmail());
			usr.setEmail2(user.getEmail2());
			usr.setName(user.getName());
			usr.setFirstName(user.getFirstName());
			usr.setSecondFactor(user.getSecondFactor());

			Tenant tenant = TenantDAO.get().findById(user.getTenantId());

			ContextProperties config = Context.get().getProperties();
			usr.setPasswordMinLenght(config.getInt(tenant.getName() + ".password.size", 6));

			// Retrieve the reason for the last login failure
			List<UserHistory> failures = UserHistoryDAO.get().findByUserIdAndEvent(user.getId(),
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
			TicketDAO ticketDao = TicketDAO.get();
			ticketDao.store(ticket);

			// Try to clean the DB from old tickets
			ticketDao.deleteExpired();

			Locale locale = user.getLocale();

			EMail email = new EMail();
			email.setHistoricyze(false);
			email.setHtml(true);
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
		UserDAO userDao = UserDAO.get();
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
		UserDAO userDao = UserDAO.get();
		User user;
		try {
			user = pickUser(username);
			userDao.initialize(user);
		} catch (PersistenceException | ServerException e) {
			log.warn(e.getMessage(), e);
			return false;
		}

		if (StringUtils.isEmpty(user.getSecondFactor()))
			return false;

		String tenant = Tenant.SYSTEM_NAME;
		try {
			tenant = TenantDAO.get().getTenantName(user.getTenantId());
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
			try {
				return !DeviceDAO.get().isTrustedDevice(user.getUsername(), request);
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

		String tenant = Tenant.DEFAULT_NAME;
		try {
			tenant = TenantDAO.get().getTenantName(user.getTenantId());
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

	@Override
	public List<GUIParameter> getLegalsToConfirm(String username) throws ServerException {
		try {
			List<GUIParameter> legals = new ArrayList<>();
			UserDAO userDao = UserDAO.get();
			userDao.queryForResultSet(
					"select ld_name, ld_title from ld_legal where not exists (select * from ld_legal_confirmation where ld_username = :username and ld_legal=ld_name) order by ld_sort",
					Map.of("username", username), null, rows -> {
						while (rows.next())
							legals.add(new GUIParameter(rows.getString(1), rows.getString(2)));
					});
			return legals;
		} catch (PersistenceException e) {
			throw new ServerException(e.getMessage());
		}
	}

	@Override
	public void confirmLegal(String username, String legal) throws ServerException {
		UserDAO userDao = UserDAO.get();
		User user;
		// Record the confirmation
		try {
			user = userDao.findByUsername(username);
			userDao.jdbcUpdate(
					"insert into ld_legal_confirmation(ld_username, ld_user, ld_legal, ld_date) values (:username, :user, :legal, CURRENT_TIMESTAMP)",
					Map.of("username", user.getUsername(), "user", user.getFullName(), "legal", legal));
			log.info("User {} confirmed legal {}", username, legal);
		} catch (PersistenceException e) {
			throw new ServerException(e.getMessage());
		}

		// Save also the user's event
		try {
			UserHistory event = new UserHistory();
			event.setUser(user);
			event.setEvent(UserEvent.LEGAL_CONFIRMED);
			event.setComment(UserHistoryDAO.get().queryForString("select ld_title from ld_legal where ld_name = :legal",
					Map.of("legal", legal)) + " - " + legal);
			UserHistoryDAO.get().store(event);
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}
	}
}