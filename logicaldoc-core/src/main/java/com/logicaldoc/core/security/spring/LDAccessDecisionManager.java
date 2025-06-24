package com.logicaldoc.core.security.spring;

import java.util.Collection;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.vote.UnanimousBased;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.core.Authentication;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Our customization of an <code>AffirmativeBased</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
@Deprecated
public class LDAccessDecisionManager extends UnanimousBased {

	private static final Logger log = LoggerFactory.getLogger(LDAccessDecisionManager.class);

	static final String REQUESTED_URL = "RequestedUrl";

	public LDAccessDecisionManager(List<AccessDecisionVoter<? extends Object>> decisionVoters) {
		super(decisionVoters);
	}

	@Override
	public void decide(Authentication authentication, Object object, Collection<ConfigAttribute> properties)
			throws AccessDeniedException, InsufficientAuthenticationException {

		if (authentication instanceof AnonymousAuthenticationToken) {
			HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes())
					.getRequest();
			if ("login".equals(request.getParameter("anonymous"))) {
				String tenant = "default";
				if (StringUtils.isNotEmpty(request.getParameter("tenant")))
					tenant = request.getParameter("tenant");

				ContextProperties config = Context.get().getProperties();
				boolean enabled = "true".equals(config.get(tenant + ".anonymous.enabled"));
				if (enabled) {
					return;
				}
			}
		}

		try {
			super.decide(authentication, object, properties);
		} catch (AccessDeniedException ade) {
			saveOriginalUrl();
			throw ade;
		} catch (InsufficientAuthenticationException iae) {
			saveOriginalUrl();
			throw iae;
		}
	}

	/**
	 * Saves the originally required URL that needs authentication
	 */
	private void saveOriginalUrl() {
		HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes())
				.getRequest();
		String url = getFullURL(request);
		request.getSession(true).setAttribute(REQUESTED_URL, url);
		log.debug("Resource {} requires authentication", url);
	}

	private String getFullURL(HttpServletRequest request) {
		StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
		String queryString = request.getQueryString();

		if (queryString == null) {
			return requestURL.toString();
		} else {
			return requestURL.append('?').append(queryString).toString();
		}
	}

	@Override
	public boolean supports(Class<?> arg0) {
		return true;
	}

	@Override
	public boolean supports(ConfigAttribute arg0) {
		return true;
	}
}
