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

	private boolean anonymous = false;

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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((sid == null) ? 0 : sid.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		LDAuthenticationToken other = (LDAuthenticationToken) obj;
		if (sid == null) {
			if (other.sid != null)
				return false;
		} else if (!sid.equals(other.sid))
			return false;
		return true;
	}

}