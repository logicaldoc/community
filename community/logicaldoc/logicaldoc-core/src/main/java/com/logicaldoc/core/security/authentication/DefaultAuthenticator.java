package com.logicaldoc.core.security.authentication;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.HibernateUserDAO;
import com.logicaldoc.core.security.dao.UserDAO;

/**
 * This is the basic authentication mechanism, that searches for the user in the
 * LogicalDOC database.
 * 
 * @author Sebastian Wenzky
 * @since 4.5
 */
public class DefaultAuthenticator extends AbstractAuthenticator {

	private static Logger log = LoggerFactory.getLogger(DefaultAuthenticator.class);

	protected UserDAO userDAO;

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	@Override
	public User authenticate(String username, String password) throws AuthenticationException {
		return authenticate(username, password, null, null);
	}

	@Override
	public User authenticate(String username, String password, String key, Client client)
			throws AuthenticationException {
		User user = pickUser(username);

		if (user == null) {
			log.debug("User {} not found in local database", username);
			throw new AccountNotFoundException(this);
		}

		validateUser(user);

		if (!userDAO.validateUser(username, password))
			throw new WrongPasswordException(this);

		return user;
	}

	@Override
	public User pickUser(String username) {
		User user = null;
		if (HibernateUserDAO.ignoreCaseLogin())
			user = userDAO.findByUsernameIgnoreCase(username);
		else
			user = userDAO.findByUsername(username);
		return user;
	}

	/**
	 * Perform some security validations on the user but does not check the
	 * password.
	 * 
	 * @param user The user to validate
	 * 
	 * @throws AuthenticationException I something did not pass
	 */
	public void validateUser(User user) throws AuthenticationException {
		if (user == null)
			throw new AccountNotFoundException();

		userDAO.initialize(user);

		if (user.getEnabled() == 0)
			throw new AccountDisabledException(this);

		if (userDAO.isPasswordExpired(user.getUsername()))
			throw new PasswordExpiredException(this);

		if (user.isExpired())
			throw new AccountExpiredException(user.getExpire());

		if (userDAO.isInactive(user.getUsername()))
			throw new AccountInactiveException(this);

		if (user.getEnforceWorkingTime() == 1 && !user.isInWorkingTime())
			throw new OutsideWorkingTimeException(this);
	}
}