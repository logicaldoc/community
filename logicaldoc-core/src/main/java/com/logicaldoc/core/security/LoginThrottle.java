package com.logicaldoc.core.security;

import java.util.Calendar;
import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authentication.IPBlockedException;
import com.logicaldoc.core.security.authentication.UsernameBlockedException;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.security.dao.UserHistoryDAO;
import com.logicaldoc.core.sequence.Sequence;
import com.logicaldoc.core.sequence.SequenceDAO;
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
			SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
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
	 * @param client the client address from which the login intent comes from
	 * @param exception the authentication exception
	 */
	public static void recordFailure(String username, Client client, AuthenticationException exception) {
		if (exception == null || !exception.mustRecordFailure())
			return;

		// Update the failed login counters
		if (Context.get().getProperties().getBoolean(THROTTLE_ENABLED)) {
			SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			if (StringUtils.isNotEmpty(username)) {
				sDao.next(LOGINFAIL_USERNAME + username, 0L, Tenant.SYSTEM_ID);
			}
			if (StringUtils.isNotEmpty(client.getAddress()))
				sDao.next(LOGINFAIL_IP + client.getAddress(), 0L, Tenant.SYSTEM_ID);
		}

		// Record the failed login attempt
		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
		try {
			User user = uDao.findByUsername(username);
			if (user == null) {
				user = new User();
				user.setUsername(username);
				user.setName(username);
			}
			UserHistoryDAO dao = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
			dao.createUserHistory(user, UserEvent.LOGIN_FAILED.toString(), exception.getMessage(), null, client);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}

	/**
	 * Performs anti brute force attack checks
	 * 
	 * @param username the username
	 * @param ip the IP address from which the login intent comes from
	 * 
	 * @throws AuthenticationException if the authentication fails
	 */
	public static void checkLoginThrottle(String username, String ip) throws AuthenticationException {
		if (!Context.get().getProperties().getBoolean(THROTTLE_ENABLED))
			return;

		if ("admin".equals(username) && ("127.0.0.1".equals(ip) || "::1".equals(ip)))
			return;

		log.debug("Take anti brute force attack countermeasures");

		// Check if the username is temporarily blocked
		checkUsername(username);

		// Check if the IP is temporarily blocked
		checkIp(ip);
	}

	private static void checkIp(String ip) throws IPBlockedException {
		SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
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
		SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
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

	protected static void disableUser(String username) {
		if (Context.get().getProperties().getBoolean("throttle.username.disableuser", false)) {
			try {
				UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
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

	private static void deleteSequence(Sequence seq) {
		try {
			SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			sDao.delete(seq.getId());
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}
	}
}