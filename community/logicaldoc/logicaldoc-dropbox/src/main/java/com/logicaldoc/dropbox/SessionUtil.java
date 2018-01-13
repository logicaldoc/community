package com.logicaldoc.dropbox;

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * Various methods related to the user session
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class SessionUtil {
	public static final String LOCALE = "locale";

	public static Session validateSession(HttpServletRequest request) throws InvalidSessionException {
		String sid = SessionManager.get().getSessionId(request);
		return validateSession(sid);
	}

	/**
	 * Throws a runtime exception id the given session is invalid
	 * 
	 * @throws InvalidSessionException
	 */
	public static Session validateSession(String sid) throws InvalidSessionException {
		Session session = SessionManager.get().get(sid);
		if (session == null)
			throw new InvalidSessionException("Invalid Session");
		if (!SessionManager.get().isOpen(sid))
			throw new InvalidSessionException("Invalid or Expired Session");
		SessionManager.get().renew(sid);
		return session;
	}

	public static Locale currentLocale(Session session) throws ServerException {
		return (Locale) session.getDictionary().get(LOCALE);
	}

	public static Locale currentLocale(String sid) throws ServerException {
		Session session = validateSession(sid);
		return currentLocale(session);
	}
}