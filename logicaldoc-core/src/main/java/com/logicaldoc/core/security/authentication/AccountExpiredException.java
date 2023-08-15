package com.logicaldoc.core.security.authentication;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Raised when the account is expired
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class AccountExpiredException extends AuthenticationException {
	private static final long serialVersionUID = 1L;

	private final Date date;

	public AccountExpiredException(Date date) {
		super("accountexpired");
		this.date = date;
	}
	
	public Date getDate() {
		return date;
	}

	public String getFormattedDate() {
		if (date == null)
			return "";
		else {
			SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			return df.format(date);
		}
	}

	@Override
	public String getMessage() {
		if (date == null)
			return super.getMessage();
		else
			return super.getMessage() + " - " + getFormattedDate();
	}
}
