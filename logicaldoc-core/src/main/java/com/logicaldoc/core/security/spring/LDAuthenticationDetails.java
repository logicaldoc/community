package com.logicaldoc.core.security.spring;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.springframework.security.core.SpringSecurityCoreVersion;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

/**
 * Our customization of an <code>AuthenticationDetails</code> used to extract a
 * third authentication parameter for the 2FA.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class LDAuthenticationDetails extends WebAuthenticationDetails {

	private static final long serialVersionUID = SpringSecurityCoreVersion.SERIAL_VERSION_UID;

	private String secretKey;

	public String getSecretKey() {
		return secretKey;
	}

	public LDAuthenticationDetails(HttpServletRequest request) {
		super(request);
		if (StringUtils.isNotEmpty(request.getParameter("j_secretkey")))
			secretKey = request.getParameter("j_secretkey");
		else
			secretKey  = request.getParameter("key");
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((secretKey == null) ? 0 : secretKey.hashCode());
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
		LDAuthenticationDetails other = (LDAuthenticationDetails) obj;
		if (secretKey == null) {
			if (other.secretKey != null)
				return false;
		} else if (!secretKey.equals(other.secretKey))
			return false;
		return true;
	}
}