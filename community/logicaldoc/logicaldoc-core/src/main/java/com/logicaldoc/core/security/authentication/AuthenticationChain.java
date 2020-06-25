package com.logicaldoc.core.security.authentication;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
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
public class AuthenticationChain extends AbstractAuthenticator {

	private static Logger log = LoggerFactory.getLogger(AuthenticationChain.class);

	private List<Authenticator> authenticators = new ArrayList<Authenticator>();

	@Override
	public final User authenticate(String username, String password, String key) throws AuthenticationException {
		return authenticate(username, password, key, null);
	}

	@Override
	public final User authenticate(String username, String password) throws AuthenticationException {
		return authenticate(username, password, null, null);
	}

	@Override
	public final User authenticate(String username, String password, String key, Client client)
			throws AuthenticationException {
		if (authenticators == null || authenticators.isEmpty())
			init();

		User user = checkAnonymousLogin(username, key);
		if (user != null)
			return user;

		List<AuthenticationException> errors = new ArrayList<AuthenticationException>();
		for (Authenticator cmp : authenticators) {
			if (!cmp.isEnabled())
				continue;

			// Validates an user for valid login credentials if a specific
			// component handles this user explicitly (e.g. admin is
			// DefaultAuthentication)
			if (cmp.canAuthenticateUser(username)) {
				try {
					user = cmp.authenticate(username, password);
				} catch (AuthenticationException ae) {
					errors.add(ae);
				}
			}

			if (user != null)
				break;
		}

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		if (user != null)
			userDao.initialize(user);
		else if (!errors.isEmpty())
			throw errors.get(0);

		return user;
	}

	/*
	 * Checks the anonymous login
	 */
	protected User checkAnonymousLogin(String username, String key) {
		String tenant = Tenant.DEFAULT_NAME;

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDao.getUser(username);

		if (user.getEnabled() == 0)
			throw new AccountDisabledException();

		if (userDao.isPasswordExpired(username))
			throw new PasswordExpiredException();

		TenantDAO tdao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant t = tdao.findById(user.getTenantId());
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
	private void init() {
		Context context = Context.get();
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Authentication");

		// Sort the extensions according to ascending position
		List<Extension> sortedExts = new ArrayList<Extension>();
		for (Extension extension : exts) {
			sortedExts.add(extension);
		}
		Collections.sort(sortedExts, new Comparator<Extension>() {
			public int compare(Extension e1, Extension e2) {
				int position1 = Integer.parseInt(e1.getParameter("position").valueAsString());
				int position2 = Integer.parseInt(e2.getParameter("position").valueAsString());
				if (position1 < position2)
					return -1;
				else if (position1 > position2)
					return 1;
				else
					return 0;
			}
		});

		for (Extension extension : sortedExts) {
			// Retrieve the authenticator bean id
			authenticators.add((Authenticator) context.getBean(extension.getParameter("authenticatorId")
					.valueAsString()));
		}

		if (sortedExts.isEmpty())
			authenticators.add((Authenticator) context.getBean(DefaultAuthenticator.class));

		for (Authenticator auth : authenticators)
			log.info("Added authenticator {}", auth.getClass().getSimpleName());
		log.info("Authentication chain initialized");
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}