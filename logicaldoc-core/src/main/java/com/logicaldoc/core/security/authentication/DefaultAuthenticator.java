package com.logicaldoc.core.security.authentication;

import java.security.NoSuchAlgorithmException;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.crypt.CryptUtil;

/**
 * This is the basic authentication mechanism, that searches for the user in the
 * LogicalDOC database.
 * 
 * @author Sebastian Wenzky
 * @since 4.5
 */
@Component("DefaultAuthenticator")
public class DefaultAuthenticator extends AbstractAuthenticator {

	protected static Logger log = LoggerFactory.getLogger(DefaultAuthenticator.class);

	@Resource(name = "UserDAO")
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
		User user;
		try {
			user = pickUser(username);
		} catch (PersistenceException e) {
			throw new AuthenticationException(this, "dataerror", e);
		}

		validateUser(user);

		// Check the password match with one of the current or legacy algorithm
		String test = null;
		String testLegacy = null;
		try {
			test = CryptUtil.encryptSHA256(password);
			testLegacy = CryptUtil.encryptSHA(password);
		} catch (NoSuchAlgorithmException e) {
			log.error(e.getMessage(), e);
		}

		if (user.getPassword() == null || (!user.getPassword().equals(test) && !user.getPassword().equals(testLegacy)))
			throw new WrongPasswordException(this);

		try {
			// Make sure the password in the DB follows the current scheme
			if (StringUtils.isNotEmpty(test) && user.getPassword().equals(testLegacy))
				userDAO.jdbcUpdate("update ld_user set ld_password='" + test + "' where ld_id = " + user.getId());
		} catch (PersistenceException e) {
			log.warn(e.getMessage());
		}

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
		if (user == null) {
			throw new AccountNotFoundException(this);
		}

		// Check the type
		if (user.getType() != User.TYPE_DEFAULT && user.getType() != User.TYPE_READONLY
				&& !"ldstaff".equals(user.getUsername()))
			throw new AccountTypeNotAllowedException();

		userDAO.initialize(user);

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