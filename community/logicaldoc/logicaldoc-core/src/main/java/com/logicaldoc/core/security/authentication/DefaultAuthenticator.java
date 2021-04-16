package com.logicaldoc.core.security.authentication;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
		User user = pickUser(username);

		if (user == null) {
			log.debug("User {} not found in local database", username);
			throw new AccountNotFoundException();
		}

		if (user.getEnabled() == 0)
			throw new AccountDisabledException();
		
		if (userDAO.isPasswordExpired(username))
			throw new PasswordExpiredException();

		if (!userDAO.validateUser(username, password))
			throw new WrongPasswordException();

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
}