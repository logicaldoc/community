package com.logicaldoc.core.security.authentication;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * This authentication component implements a chain of possible authentication
 * sources that will be invoked sequentially.
 * 
 * @author Sebastian Wenzky
 * @since 4.5
 */
@Component("AuthenticationChain")
public class AuthenticationChain extends AbstractAuthenticator {

	private static Logger log = LoggerFactory.getLogger(AuthenticationChain.class);

	private List<Authenticator> authenticators = new ArrayList<>();

	@Override
	public final User authenticate(String username, String password) throws AuthenticationException {
		return authenticate(username, password, null, null);
	}

	@Override
	public final User authenticate(String username, String password, String key, Client client)
			throws AuthenticationException {
		init();

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		User user = validateAnonymousUser(username, key, client);
		if (user != null)
			return user;

		List<AuthenticationException> errors = new ArrayList<>();
		user = authenticateUsingAuthenticators(username, password, key, client, user, errors);

		/*
		 * At the end we need to do in any case some default validations
		 */
		try {
			defaultValidations(username, client);
		} catch (AuthenticationException ae) {
			errors.clear();
			errors.add(ae);
			user = null;
		} catch (PersistenceException pe) {
			log.error(pe.getMessage(), pe);
			errors.clear();
			user = null;
		}

		log.debug("Collected authentication errors: {}", errors);

		if (user != null) {
			userDao.initialize(user);
		} else if (!errors.isEmpty()) {
			// In case of multiple errors, we consider the first one that is
			// not a UserNotFound exception because it is normal that some
			// authenticator does not find this user because not in it's domain
			if (errors.size() > 1)
				for (AuthenticationException err : errors)
					if (!(err instanceof AccountNotFoundException))
						throw err;
			throw errors.get(0);
		}

		return user;
	}

	private User validateAnonymousUser(String username, String key, Client client) {
		User user = null;
		try {
			user = checkAnonymousLogin(username, key, client);
		} catch (AuthenticationException | PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return user;
	}

	private User authenticateUsingAuthenticators(String username, String password, String key, Client client, User user,
			List<AuthenticationException> errors) {
		for (Authenticator cmp : authenticators) {
			if (cmp.isEnabled()) {
				// Validates an user for valid login credentials if a specific
				// component handles this user explicitly (e.g. admin is
				// DefaultAuthentication)
				if (cmp.canAuthenticateUser(username)) {
					try {
						user = cmp.authenticate(username, password, key, client);
					} catch (AuthenticationException ae) {
						errors.add(ae);
					}
				}

				if (user != null)
					break;
			}
		}
		return user;
	}

	protected void defaultValidations(String username, Client client)
			throws AuthenticationException, PersistenceException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDao.findByUsername(username);
		if (user == null)
			return;

		DefaultAuthenticator defaultValidator = (DefaultAuthenticator) Context.get()
				.getBean(DefaultAuthenticator.class);
		try {
			defaultValidator.validateUser(user);
		} catch (AccountInactiveException ie) {
			userDao.initialize(user);
			UserHistory transaction = new UserHistory();
			transaction.setUser(user);
			transaction.setClient(client);
			transaction.setEvent(UserEvent.DISABLED.toString());
			transaction.setComment("inactive for too many days");

			user.setEnabled(0);
			userDao.store(user, transaction);

			throw ie;
		}
	}

	@Override
	public User pickUser(String username) {
		init();

		User user = null;
		for (Authenticator cmp : authenticators) {
			if (cmp.isEnabled()) {
				// Validates an user for valid login credentials if a specific
				// component handles this user explicitly (e.g. admin is
				// DefaultAuthentication)
				if (cmp.canAuthenticateUser(username)) {
					try {
						user = cmp.pickUser(username);
					} catch (Exception t) {
						log.warn("Cannot pick user {} using authenticator {}", username, cmp.getClass().getName(), t);
					}
				}

				if (user != null)
					break;
			}
		}

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		if (user != null)
			userDao.initialize(user);
		return user;
	}

	/*
	 * Checks the anonymous login
	 */
	protected User checkAnonymousLogin(String username, String key, Client client)
			throws AuthenticationException, PersistenceException {
		String tenant = Tenant.DEFAULT_NAME;

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDao.getUser(username);

		defaultValidations(username, client);

		TenantDAO tdao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant t = user != null ? tdao.findById(user.getTenantId()) : null;
		if (t != null)
			tenant = t.getName();

		if (key != null) {
			ContextProperties config = Context.get().getProperties();
			if ("true".equals(config.getProperty(tenant + ".anonymous.enabled"))
					&& username.equals(config.getProperty(tenant + ".anonymous.user"))
					&& key.equals(config.getProperty(tenant + ".anonymous.key")))
				return user;
		}

		return null;
	}

	@Override
	public boolean canAuthenticateUser(String user) {
		return false;
	}

	/**
	 * Populate the authenticators chain using the extension point
	 * Authentication declared in the core plug-in.
	 */
	public synchronized void init() {
		if (!authenticators.isEmpty())
			return;

		Context context = Context.get();
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Authentication");

		// Sort the extensions according to ascending position
		List<Extension> sortedExts = new ArrayList<>();
		for (Extension extension : exts) {
			sortedExts.add(extension);
		}
		Collections.sort(sortedExts, (e1, e2) -> {
			int position1 = Integer.parseInt(e1.getParameter("position").valueAsString());
			int position2 = Integer.parseInt(e2.getParameter("position").valueAsString());
			if (position1 < position2)
				return -1;
			else if (position1 > position2)
				return 1;
			else
				return 0;
		});

		for (Extension extension : sortedExts) {
			// Retrieve the authenticator bean id
			authenticators
					.add((Authenticator) context.getBean(extension.getParameter("authenticatorId").valueAsString()));
		}

		if (sortedExts.isEmpty())
			authenticators.add((Authenticator) context.getBean(DefaultAuthenticator.class));

		for (Authenticator auth : authenticators) {
			log.warn("Added authenticator {}", auth.getClass().getSimpleName());
		}
		log.warn("Authentication chain initialized");
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}