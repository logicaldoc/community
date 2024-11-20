package com.logicaldoc.core.security;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.authentication.ApiKeyBlockedException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authentication.IPBlockedException;
import com.logicaldoc.core.security.authentication.UsernameBlockedException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.core.sequence.Sequence;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Utility methods to prevent brute force attacks
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class LoginThrottle {
	private static final String THROTTLE_ENABLED = "throttle.enabled";

	public static final String LOGINFAIL_APIKEY = "loginfail-apikey-";

	public static final String LOGINFAIL_IP = "loginfail-ip-";

	public static final String LOGINFAIL_USERNAME = "loginfail-username-";

	protected static Logger log = LoggerFactory.getLogger(LoginThrottle.class);

	private LoginThrottle() {
	}

	/**
	 * Clears the failures for the given username and or password
	 * 
	 * @param username the username
	 * @param ip the IP address from which the login intent comes from
	 */
	public static void clearFailures(String username, String ip) {
		if (Context.get().getProperties().getBoolean(THROTTLE_ENABLED)) {
			SequenceDAO sDao = Context.get().getBean(SequenceDAO.class);
			if (StringUtils.isNotEmpty(username))
				try {
					sDao.delete(LOGINFAIL_USERNAME + username, 0L, Tenant.SYSTEM_ID);
				} catch (PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
			if (StringUtils.isNotEmpty(ip))
				try {
					sDao.delete(LOGINFAIL_IP + ip, 0L, Tenant.SYSTEM_ID);
				} catch (PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
		}
	}

	/**
	 * Saves the login failure in the database
	 * 
	 * @param username the username
	 * @param apiKey the API Key
	 * @param client the client address from which the login intent comes from
	 * @param exception the authentication exception
	 */
	public static void recordFailure(String username, String apiKey, Client client, AuthenticationException exception) {
		if (exception == null || !exception.mustRecordFailure())
			return;

		// Update the failed login counters
		if (Context.get().getProperties().getBoolean(THROTTLE_ENABLED)) {
			SequenceDAO sDao = Context.get().getBean(SequenceDAO.class);
			if (StringUtils.isNotEmpty(username))
				sDao.next(LOGINFAIL_USERNAME + username, 0L, Tenant.SYSTEM_ID);
			if (StringUtils.isNotEmpty(client.getAddress()))
				sDao.next(LOGINFAIL_IP + client.getAddress(), 0L, Tenant.SYSTEM_ID);
			if (StringUtils.isNotEmpty(apiKey))
				sDao.next(LOGINFAIL_APIKEY + apiKey, 0L, Tenant.SYSTEM_ID);
		}

		// Record the failed login attempt
		UserDAO uDao = Context.get().getBean(UserDAO.class);
		try {
			User user = uDao.findByUsername(username);
			if (user == null) {
				user = new User();
				user.setUsername(username);
				user.setName(username);
			}
			UserHistoryDAO dao = Context.get().getBean(UserHistoryDAO.class);
			dao.createUserHistory(user, UserEvent.LOGIN_FAILED.toString(), exception.getMessage(), null, client);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}

	/**
	 * Performs anti brute force attack checks
	 * 
	 * @param username the username
	 * @param apikey the API Key
	 * @param ip the IP address from which the login intent comes from
	 * 
	 * @throws AuthenticationException if the authentication fails
	 */
	public static void checkLoginThrottle(String username, String apikey, String ip) throws AuthenticationException {
		if (!Context.get().getProperties().getBoolean(THROTTLE_ENABLED))
			return;

		if ("admin".equals(username) && ("127.0.0.1".equals(ip) || "::1".equals(ip)))
			return;

		log.debug("Take anti brute force attack countermeasures");

		// Check if the username is temporarily blocked
		checkUsername(username);

		// Check if the IP is temporarily blocked
		checkIp(ip);

		// Check if the IP is temporarily blocked
		checkApikey(apikey);
	}

	private static void checkIp(String ip) throws IPBlockedException {
		SequenceDAO sDao = Context.get().getBean(SequenceDAO.class);
		Calendar cal = Calendar.getInstance();

		ContextProperties config = Context.get().getProperties();
		int wait = config.getInt("throttle.ip.wait", 0);
		int maxTrials = config.getInt("throttle.ip.max", 0);

		if (maxTrials > 0 && wait > 0) {
			String counterName = LOGINFAIL_IP + ip;
			Sequence seq = sDao.findByAlternateKey(counterName, 0L, Tenant.SYSTEM_ID);
			if (seq != null) {
				long count = seq.getValue();
				if (count >= maxTrials) {
					cal.add(Calendar.MINUTE, -wait);
					Date oldestDate = cal.getTime();
					if (oldestDate.before(seq.getLastModified())) {
						log.warn("Possible brute force attack detected for IP {}", ip);
						notifyBruteForceAttack(null, ip);
						throw new IPBlockedException();
					} else {
						log.info("Login block for IP {} expired", ip);
						deleteSequence(seq);
					}
				}
			}
		}
	}

	private static void checkUsername(String username) throws UsernameBlockedException {
		SequenceDAO sDao = Context.get().getBean(SequenceDAO.class);
		Calendar cal = Calendar.getInstance();

		ContextProperties config = Context.get().getProperties();
		int wait = config.getInt("throttle.username.wait", 0);
		int maxTrials = config.getInt("throttle.username.max", 0);

		if (maxTrials > 0 && wait > 0) {
			String counterName = LOGINFAIL_USERNAME + username;
			Sequence seq = sDao.findByAlternateKey(counterName, 0L, Tenant.SYSTEM_ID);
			if (seq != null) {
				long count = seq.getValue();
				if (count >= maxTrials) {
					cal.add(Calendar.MINUTE, -wait);
					Date oldestDate = cal.getTime();
					if (oldestDate.before(seq.getLastModified())) {
						log.warn("Possible brute force attack detected for username {}", username);
						notifyBruteForceAttack(username, null);

						disableUser(username);

						throw new UsernameBlockedException();
					} else {
						log.info("Login block for username {} expired", username);
						deleteSequence(seq);
					}
				}
			}
		}
	}

	private static void checkApikey(String apikey) throws ApiKeyBlockedException {
		if (StringUtils.isEmpty(apikey))
			return;

		SequenceDAO sDao = Context.get().getBean(SequenceDAO.class);
		Calendar cal = Calendar.getInstance();

		ContextProperties config = Context.get().getProperties();
		int wait = config.getInt("throttle.apikey.wait", 0);
		int maxTrials = config.getInt("throttle.apikey.max", 0);

		if (maxTrials > 0 && wait > 0) {
			String counterName = LOGINFAIL_APIKEY + apikey;
			Sequence seq = sDao.findByAlternateKey(counterName, 0L, Tenant.SYSTEM_ID);
			if (seq != null) {
				long count = seq.getValue();
				if (count >= maxTrials) {
					cal.add(Calendar.MINUTE, -wait);
					Date oldestDate = cal.getTime();
					if (oldestDate.before(seq.getLastModified())) {
						log.warn("Possible brute force attack detected for ApiKey {}", apikey);
						notifyBruteForceAttack(null, apikey);
						throw new ApiKeyBlockedException();
					} else {
						log.info("Login block for ApiKey {} expired", apikey);
						deleteSequence(seq);
					}
				}
			}
		}
	}

	protected static void disableUser(String username) {
		if (Context.get().getProperties().getBoolean("throttle.username.disableuser", false)) {
			try {
				UserDAO userDao = Context.get().getBean(UserDAO.class);
				User user = userDao.findByUsername(username);
				if (user != null && user.getEnabled() == 1) {
					user.setEnabled(0);

					UserHistory transaction = new UserHistory();
					transaction.setEvent(UserEvent.DISABLED.toString());
					transaction.setUser(user);
					transaction.setComment("too many login failures");

					userDao.store(user, transaction);
				}
			} catch (PersistenceException e) {
				log.warn("Error trying to disable user{}", username, e);
			}
		}
	}

	private static void notifyBruteForceAttack(String suspectedUsername, String suspectedIp) {
		ThreadPools.get().schedule(() -> {
			try {
				Date date = new Date();
				List<User> recipients = getRecipients();
				for (User user : recipients) {
					log.debug("Prepare Brute Force Attack Alert for user {}", user);

					Map<String, Object> dictionary = new HashMap<>();
					dictionary.put("suspectedUsername", suspectedUsername);
					dictionary.put("suspectedIp", suspectedIp);
					dictionary.put("date", date);
					dictionary.put(Automation.LOCALE, user.getLocale());

					MessageTemplateDAO templateDAO = Context.get().getBean(MessageTemplateDAO.class);
					MessageTemplate template = templateDAO.findByNameAndLanguage("bfa.alert", user.getLanguage(),
							Tenant.DEFAULT_ID);
					if (template == null)
						template = templateDAO.findByNameAndLanguage("bfa.alert", "en", Tenant.DEFAULT_ID);

					String subject = template.getFormattedSubject(dictionary);
					String body = template.getFormattedBody(dictionary);

					/*
					 * Save a system message
					 */
					SystemMessage message = new SystemMessage();
					message.setTenantId(user.getTenantId());
					message.setType(Message.TYPE_SYSTEM);
					message.setHtml(1);
					message.setAuthor("SYSTEM");
					message.setLocale(user.getLocale());
					message.setMessageText(body);
					message.setSubject(subject);

					Recipient recipient = new Recipient();
					recipient.setAddress(user.getEmail());
					recipient.setName(user.getUsername());
					recipient.setType(Recipient.TYPE_SYSTEM);
					message.getRecipients().add(recipient);

					SystemMessageDAO messageDAO = Context.get().getBean(SystemMessageDAO.class);
					messageDAO.store(message);

					/*
					 * Send an e-mail message
					 */
					EMail email = new EMail();
					email.setTenantId(message.getTenantId());
					email.setHtml(message.getHtml());
					email.setLocale(message.getLocale());
					email.setAuthor(message.getAuthor());
					email.setSubject(message.getSubject());
					email.setMessageText(message.getMessageText());
					recipient = new Recipient();
					recipient.setAddress(user.getEmail());
					recipient.setName(user.getFullName());
					recipient.setMode(Recipient.MODE_EMAIL_TO);
					email.getRecipients().add(recipient);

					EMailSender sender = Context.get().getBean(EMailSender.class);
					sender.sendAsync(email);
				}
			} catch (PersistenceException | AutomationException e) {
				log.warn(e.getMessage(), e);
			}
		}, "BruteForceAttack", 500);
	}

	private static List<User> getRecipients() throws PersistenceException {
		List<User> recipients = new ArrayList<>();
		String setting = Context.get().getProperties().getProperty("throttle.alert.recipients", "");
		if (StringUtils.isNotEmpty(setting)) {
			UserDAO uDao = Context.get().getBean(UserDAO.class);
			String[] usernames = setting.split(",");
			for (String username : usernames) {
				User user = uDao.findByUsername(username);
				if (user != null)
					recipients.add(user);
			}

		}

		return recipients;
	}

	private static void deleteSequence(Sequence seq) {
		try {
			SequenceDAO sDao = Context.get().getBean(SequenceDAO.class);
			sDao.delete(seq.getId());
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}
}