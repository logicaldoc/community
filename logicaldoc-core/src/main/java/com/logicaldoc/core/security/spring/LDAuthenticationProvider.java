package com.logicaldoc.core.security.spring;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.CredentialsExpiredException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
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
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.util.Context;

import jakarta.servlet.http.HttpServletRequest;

/**
 * This Authentication provider users the <code>AuthenticationChain</code> to
 * authenticate the users.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationProvider implements AuthenticationProvider {

	private static final Logger log = LoggerFactory.getLogger(LDAuthenticationProvider.class);

	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException {
		if (authentication instanceof AnonymousAuthenticationToken anonymousAuthentication)
			return authenticateAnonymous(anonymousAuthentication);
		else if (authentication instanceof UsernamePasswordAuthenticationToken credentialsAuthentication)
			return authenticateCredentials(credentialsAuthentication);
		else
			return authentication;
	}

	protected Authentication authenticateCredentials(UsernamePasswordAuthenticationToken authentication) {
		String username = String.valueOf(authentication.getPrincipal());

		HttpServletRequest httpReq = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes())
				.getRequest();

		String password = getPassword(authentication, httpReq);

		String key = httpReq.getParameter("key");

		if (authentication.getDetails() instanceof LDAuthenticationDetails ldAuthenticationDetails)
			key = ldAuthenticationDetails.getSecretKey();

		log.debug("Authenticate user {} with key {}", username, key != null ? key : "-");

		Client client = SessionManager.get().buildClient(httpReq);

		// Check the passwords match
		Session session = null;
		try {
			session = SessionManager.get().newSession(username, password, key, client);

			// Preferably clear the password in the user object before storing
			// in authentication object
			session.getUser().clearPassword();

			Collection<GrantedAuthority> authorities = new ArrayList<>();
			for (String role : session.getUser().getGroups().stream().map(Group::getName).toList()) {
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

			// Hide the real exception reason to not disclose that the username
			// doesn't exist
			throw new CredentialsExpiredException("Bad credentials");
		} catch (AccountDisabledException ad) {
			String message = String.format("User %s is disabled", username);
			log.warn(message);

			throw new DisabledException(ad.getMessage());
		} catch (PasswordExpiredException pe) {
			String message = String.format("Credentials expired for user %s", username);
			log.warn(message);

			throw new CredentialsExpiredException(pe.getMessage());
		} catch (AccountExpiredException aee) {
			String message = String.format("User %s expired on %s", username, aee.getDate());
			log.warn(message);

			throw new CredentialsExpiredException(aee.getMessage());
		} catch (AccountInactiveException aie) {
			String message = String.format(
					"User %s was considered inactive because there were no interactions for too many days", username);
			log.warn(message);

			throw new CredentialsExpiredException(aie.getMessage());
		} catch (OutsideWorkingTimeException owte) {
			String message = String.format("User %s tried to enter outside his working hours", username);
			log.warn(message);

			throw new CredentialsExpiredException(owte.getMessage());
		} catch (com.logicaldoc.core.security.authentication.AuthenticationException ae) {
			throw new CredentialsExpiredException(ae.getMessage() != null
					? String.format("Security checks failed for user %s - %s", username, ae.getMessage())
					: "badcredentials");
		} catch (Exception e) {
			throw new CredentialsExpiredException(e.getMessage() != null
					? String.format("Security checks failed for user %s - %s", username, e.getMessage())
					: "badcredentials");
		}
	}

	private AnonymousAuthenticationToken authenticateAnonymous(AnonymousAuthenticationToken authentication)
			throws AuthenticationException {
		log.debug("Authenticate anonymous user");
		HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes())
				.getRequest();
		if ("login".equals(request.getParameter("anonymous"))) {
			String tenant = "default";
			if (StringUtils.isNotEmpty(request.getParameter("tenant")))
				tenant = request.getParameter("tenant");

			authentication
					.setAuthenticated(Context.get().getProperties().getBoolean(tenant + ".anonymous.enabled", false));
		}

		return authentication;
	}

	private String getPassword(UsernamePasswordAuthenticationToken auth, HttpServletRequest httpReq) {
		String password = String.valueOf(auth.getCredentials());
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
		return password;
	}

	@Override
	public boolean supports(Class<?> arg0) {
		return true;
	}
}