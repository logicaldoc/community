package com.logicaldoc.core.security.authentication;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Raised when you try to change password that was already used
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class PasswordAlreadyUsedException extends AuthenticationException {
	private static final long serialVersionUID = 1L;

	private final Date date;

	public PasswordAlreadyUsedException(Date date) {
		super("passwordalreadyused");
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

	@Override
	public boolean mustRecordFailure() {
		return false;
	}
}
