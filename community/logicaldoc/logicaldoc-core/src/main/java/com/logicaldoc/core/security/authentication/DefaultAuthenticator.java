package com.logicaldoc.core.security.authentication;

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

	protected UserDAO userDAO;

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	@Override
	public User authenticate(String username, String password) throws AuthenticationException {
		User user = null;

		if (HibernateUserDAO.ignoreCaseLogin())
			user = userDAO.findByUsernameIgnoreCase(username);
		else
			user = userDAO.findByUsername(username);

		if (user == null)
			throw new AccountNotFoundException();

		if (user.getEnabled() == 0)
			throw new AccountDisabledException();

		if (userDAO.isPasswordExpired(username))
			throw new PasswordExpiredException();

		if (!userDAO.validateUser(username, password))
			throw new WrongPasswordException();

		return user;
	}
}