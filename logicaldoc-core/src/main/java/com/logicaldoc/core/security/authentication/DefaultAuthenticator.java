package com.logicaldoc.core.security.authentication;

import java.security.NoSuchAlgorithmException;

import jakarta.annotation.Resource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserType;
import com.logicaldoc.util.crypt.CryptUtil;

/**
 * This is the basic authentication mechanism, that searches for the user in the
 * LogicalDOC database.
 * 
 * @author Sebastian Wenzky
 * @since 4.5
 */
@Component("defaultAuthenticator")
public class DefaultAuthenticator extends AbstractAuthenticator {

	private static final Logger log = LoggerFactory.getLogger(DefaultAuthenticator.class);

	@Resource(name = "UserDAO")
	protected UserDAO userDAO;

	@Override
	public User authenticate(String username, String password) throws AuthenticationException {
		return authenticate(username, password, null, null);
	}

	@Override
	public User authenticate(String username, String password, String key, Client client)
			throws AuthenticationException {
		User user;
		try {
			user = pickUser(username);
		} catch (PersistenceException e) {
			throw new AuthenticationException(this, "dataerror", e);
		}

		validateUser(user);

		// Check the password match with one of the current or legacy algorithm
		String test = null;
		try {
			test = CryptUtil.encryptSHA256(password);
		} catch (NoSuchAlgorithmException e) {
			log.error(e.getMessage(), e);
		}

		if (user.getPassword() == null || !user.getPassword().equals(test))
			throw new WrongPasswordException(this);

		return user;
	}

	@Override
	public User pickUser(String username) throws PersistenceException {
		return userDAO.getUser(username);
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
			throw new AccountNotFoundException(this);

		// Check the type
		if (user.getType() != UserType.DEFAULT && user.getType() != UserType.READONLY)
			throw new AccountTypeNotAllowedException();

		try {
			userDAO.initialize(user);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (user.getEnabled() == 0)
			throw new AccountDisabledException(this);

		if (userDAO.isPasswordExpired(user.getUsername()))
			throw new PasswordExpiredException(this);

		if (user.isExpired())
			throw new AccountExpiredException(user.getExpire());

		try {
			if (userDAO.isInactive(user.getUsername()))
				throw new AccountInactiveException(this);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (user.getEnforceWorkingTime() == 1 && !user.isInWorkingTime())
			throw new OutsideWorkingTimeException(this);
	}
}