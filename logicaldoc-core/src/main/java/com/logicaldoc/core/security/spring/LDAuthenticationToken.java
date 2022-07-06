package com.logicaldoc.core.security.spring;

import java.util.Collection;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;

/**
 * A LogicalDOC aware authentication token
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationToken extends UsernamePasswordAuthenticationToken {

	private static final long serialVersionUID = 1L;

	public static final String COOKIE_SID = "ldoc-sid";

	public static final String PARAM_SID = "sid";

	private String sid;

	private boolean anonymous=false;

	/**
	 * Constructor for the anonymous login
	 * 
	 * @param username the username
	 */
	public LDAuthenticationToken(String username) {
		this(username, "", null);
		this.anonymous = true;
	}

	public LDAuthenticationToken(Object principal, Object credentials,
			Collection<? extends GrantedAuthority> authorities) {
		super(principal, credentials, authorities);
	}

	public LDAuthenticationToken() {
		this(null, null, null);
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}

	public boolean isAnonymous() {
		return anonymous;
	}

	@Override
	public String toString() {
		if (sid != null)
			return sid;
		else
			return super.toString();
	}

}