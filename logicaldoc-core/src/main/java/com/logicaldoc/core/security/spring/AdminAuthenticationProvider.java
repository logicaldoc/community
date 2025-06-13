package com.logicaldoc.core.security.spring;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This {@link AuthenticationProvider} checks if the provided password matches
 * the context property 'adminpswd' in cases when the database is not available
 * or the max concurrent sessions number is reached.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AdminAuthenticationProvider implements AuthenticationProvider {

	private static final Logger log = LoggerFactory.getLogger(AdminAuthenticationProvider.class);

	private static final String BADCREDENTIALS = "badcredentials";

	private static final String ADMIN = "admin";

	public AdminAuthenticationProvider() {
		super();
	}

	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException {
		UsernamePasswordAuthenticationToken auth = (UsernamePasswordAuthenticationToken) authentication;
		String username = String.valueOf(auth.getPrincipal());

		if (!ADMIN.equals(username))
			throw new BadCredentialsException(BADCREDENTIALS);

		User user = new User();
		user.setUsername(ADMIN);
		try {
			user.setDecodedPassword(String.valueOf(auth.getCredentials()));
		} catch (NoSuchAlgorithmException e) {
			log.error(e.getMessage(), e);
		}

		UserDAO uDao = Context.get(UserDAO.class);

		/**
		 * The standard authentication has failed, now check the database
		 * availability
		 */
		boolean dbAvailable = false;
		try {
			long userId = uDao.queryForLong("select ld_id from ld_user where ld_username='admin' and ld_deleted=0");
			dbAvailable = userId == 1L;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}

		String adminPasswd = null;

		if (dbAvailable) {
			// If the database is available, get the password from the db
			try {
				adminPasswd = uDao
						.queryForString("select ld_password from ld_user where ld_username='admin' and ld_deleted=0");
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		} else {
			// If the database is not available, get the password from the
			// configuration file
			try {
				ContextProperties config = Context.get().getProperties();
				adminPasswd = config.getProperty("adminpasswd");
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		}

		if (adminPasswd == null || adminPasswd.isEmpty())
			throw new BadCredentialsException(BADCREDENTIALS);

		if (!user.getPassword().equals(adminPasswd))
			throw new BadCredentialsException(BADCREDENTIALS);

		Collection<GrantedAuthority> authorities = new ArrayList<>();
		authorities.add(new SimpleGrantedAuthority(ADMIN));

		// Return an authenticated token, containing user data and
		// authorities
		return new LDAuthenticationToken(user, null, authorities);
	}

	@Override
	public boolean supports(Class<?> arg0) {
		return true;
	}
}