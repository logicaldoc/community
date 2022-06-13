package com.logicaldoc.core.security.spring;

import java.util.ArrayList;
import java.util.Collection;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.CredentialsExpiredException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.LoginThrottle;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AccountDisabledException;
import com.logicaldoc.core.security.authentication.AccountExpiredException;
import com.logicaldoc.core.security.authentication.AccountInactiveException;
import com.logicaldoc.core.security.authentication.AccountNotFoundException;
import com.logicaldoc.core.security.authentication.OutsideWorkingTimeException;
import com.logicaldoc.core.security.authentication.PasswordExpiredException;

/**
 * This Authentication provider users the <code>AuthenticationChain</code> to
 * authenticate the users.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationProvider implements AuthenticationProvider {

	private static Logger log = LoggerFactory.getLogger(LDAuthenticationProvider.class);

	public LDAuthenticationProvider() {
	}

	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException {
		UsernamePasswordAuthenticationToken auth = (UsernamePasswordAuthenticationToken) authentication;
		String username = String.valueOf(auth.getPrincipal());
		String password = String.valueOf(auth.getCredentials());

		HttpServletRequest httpReq = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes())
				.getRequest();
		String jPassword = httpReq.getParameter("j_password");
		if (jPassword != null && !jPassword.equals(password)) {
			/*
			 * When the password contains a % followed by two digits the
			 * password read by Spring Security gets wrong. For instance the
			 * password xx%89xx! is returned as xx?xx! So in case the password
			 * is different from j_password parameter, we must use the
			 * j_password
			 */
			password = jPassword;
		}

		String key = httpReq.getParameter("key");

		if (authentication.getDetails() instanceof LDAuthenticationDetails) {
			key = ((LDAuthenticationDetails) authentication.getDetails()).getSecretKey();
		}

		log.debug("Authenticate user {} with key {}", username, key != null ? key : "-");

		Client client = SessionManager.get().buildClient(httpReq);

		// Check the passwords match
		Session session = null;
		try {
			session = SessionManager.get().newSession(username, password, key, client);

			// Preferably clear the password in the user object before storing
			// in authentication object
			session.getUser().clearPassword();

			String[] groups = session.getUser().getGroupNames();
			Collection<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
			for (String role : groups) {
				authorities.add(new SimpleGrantedAuthority(role));
			}

			// Clear the login failures
			LoginThrottle.clearFailures(username, client.getAddress());

			// Return an authenticated token, containing user data and
			// authorities
			LDAuthenticationToken a = new LDAuthenticationToken(session.getUser(), null, authorities);
			a.setSid(session.getSid());

			return a;
		} catch (AccountNotFoundException nf) {
			String message = String.format("Username %s not found", username);
			log.warn(message);

			// Register a new login failure
			LoginThrottle.recordFailure(username, client, nf);

			throw new UsernameNotFoundException(nf.getMessage());
		} catch (AccountDisabledException ad) {
			String message = String.format("User %s is disabled", username);
			log.warn(message);

			// Register a new login failure
			LoginThrottle.recordFailure(username, client, ad);

			throw new DisabledException(ad.getMessage());
		} catch (PasswordExpiredException pe) {
			String message = String.format("Credentials expired for user %s", username);
			log.warn(message);

			// Register a new login failure
			LoginThrottle.recordFailure(username, client, pe);

			throw new CredentialsExpiredException(pe.getMessage());
		} catch (AccountExpiredException aee) {
			String message = String.format("User %s expired on %s", username, aee.getDate());
			log.warn(message);

			// Register a new login failure
			LoginThrottle.recordFailure(username, client, aee);

			throw new CredentialsExpiredException(aee.getMessage());
		} catch (AccountInactiveException aie) {
			String message = String.format(
					"User %s was considered inactive because there were no interactions for too many days", username);
			log.warn(message);

			// Register a new login failure
			LoginThrottle.recordFailure(username, client, aie);

			throw new CredentialsExpiredException(aie.getMessage());
		} catch (OutsideWorkingTimeException owte) {
			String message = String.format("User %s tried to enter outside his working hours", username);
			log.warn(message);

			// Register a new login failure
			LoginThrottle.recordFailure(username, client, owte);

			throw new CredentialsExpiredException(owte.getMessage());
		} catch (Throwable ae) {
			String message = String.format("Security checks failed for user %s - %s", username, ae.getMessage());
			log.warn(message);

			if (ae instanceof com.logicaldoc.core.security.authentication.AuthenticationException)
				LoginThrottle.recordFailure(username, client,
						(com.logicaldoc.core.security.authentication.AuthenticationException) ae);
			throw new CredentialsExpiredException(ae.getMessage() != null ? ae.getMessage() : "badcredentials");
		}
	}

	@Override
	public boolean supports(Class<?> arg0) {
		return true;
	}
}