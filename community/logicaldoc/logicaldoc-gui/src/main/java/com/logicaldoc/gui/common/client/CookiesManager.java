package com.logicaldoc.gui.common.client;

import com.google.gwt.user.client.Cookies;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.smartgwt.client.util.Offline;

/**
 * Here we handle the storage of informations in the broeserk
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class CookiesManager {

	public static final String COOKIE_HITSLIST = "ldoc-hitslist";

	public static final String COOKIE_HITSLIST_MODE = "ldoc-hitslist-mode";

	public static final String COOKIE_HITSLIST_PREV_W = "ldoc-hitslist-prev-w";

	public static final String COOKIE_DOCSLIST = "ldoc-docslist";

	public static final String COOKIE_DOCSLIST_SORT = "ldoc-docslist-sort";

	public static final String COOKIE_DOCSLIST_MAX = "ldoc-docslist-max";

	public static final String COOKIE_DOCSLIST_MODE = "ldoc-docslist-mode";

	public static final String COOKIE_DOCSLIST_PREV_W = "ldoc-docslist-prev-w";

	public static final String COOKIE_DOCSMENU_W = "ldoc-docsmenu-w";

	public static final String COOKIE_SAVELOGIN = "ldoc-savelogin";

	public static final String COOKIE_DENSITY = "ldoc-density";

	public static final String COOKIE_USER = "ldoc-user";

	public static final String COOKIE_PASSWORD = "ldoc-password";

	public static final String COOKIE_VERSION = "ldoc-version";

	public static final String COOKIE_SID = "ldoc-sid";

	public static final String COOKIE_FAILURE = "ldoc-failure";

	/**
	 * Removes the cookies that store the session ID
	 */
	public static void removeSid() {
		try {
			Offline.remove(COOKIE_SID);
			Cookies.removeCookie(COOKIE_SID);
		} catch (Throwable t) {

		}
	}

	/**
	 * Removes the cookies used to handle the login
	 */
	public static void removeLogin() {
		try {
			removeSid();
			Cookies.removeCookie(COOKIE_FAILURE);
		} catch (Throwable t) {

		}
	}

	/**
	 * Removes all the informations stored in the browser
	 */
	public static void removeAllCookies() {
		try {
			Offline.remove(COOKIE_DENSITY);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_HITSLIST);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_HITSLIST_PREV_W);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_DOCSLIST);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_DOCSLIST_SORT);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_DOCSLIST_MAX);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_DOCSLIST_PREV_W);
		} catch (Throwable t) {

		}

		try {
			Offline.remove(COOKIE_DOCSMENU_W);
		} catch (Throwable t) {

		}
		try {
			Offline.remove(COOKIE_PASSWORD);
		} catch (Throwable t) {

		}
		try {
			Offline.remove(COOKIE_SAVELOGIN);
		} catch (Throwable t) {

		}
		try {
			Offline.remove(COOKIE_USER);
		} catch (Throwable t) {

		}
	}

	/**
	 * Gets a cookie
	 */
	public static String get(String cookieName) {
		return (String) Offline.get(cookieName);
	}

	/**
	 * Retrieves the Session ID stored in the browser
	 */
	public static String getSid() {
		String sid = null;
		try {
			sid = Cookies.getCookie(COOKIE_SID);
		} catch (Throwable t) {
		}

		try {
			if (sid == null)
				sid = Offline.get(COOKIE_SID).toString();
		} catch (Throwable t) {
		}

		return sid;
	}

	/**
	 * Saves the cookie
	 */
	public static void save(String cookieName, String value) {
		Offline.put(cookieName, value);
	}

	public static void save(String cookieName, int value) {
		Offline.put(cookieName, value);
	}

	/**
	 * Stores the release information
	 */
	public static void saveRelease(GUIInfo info) {
		Cookies.setCookie(COOKIE_VERSION, info.getRelease(), null, null, null, true);
	}

	public static boolean isSaveLogin() {
		return "true".equals(Offline.get(COOKIE_SAVELOGIN));
	}

	/**
	 * Retrieves the credentials
	 * 
	 * @return username and password
	 */
	public static String[] getSavedCredentials() {
		try {
			return new String[] { Offline.get(COOKIE_USER).toString(), Offline.get(COOKIE_PASSWORD).toString() };
		} catch (Throwable t) {
			return new String[] { null, null };
		}
	}

	/**
	 * Retrieve the failure of the lat login
	 */
	public static String getFailure() {
		return Cookies.getCookie(COOKIE_FAILURE);
	}

	/**
	 * Saves the login informations
	 */
	public static void saveLogin(boolean saveLoginEnabled, boolean rememberMe, String username, String password) {
		if (saveLoginEnabled) {
			save(COOKIE_SAVELOGIN, Boolean.toString(rememberMe));
			save(CookiesManager.COOKIE_USER, rememberMe ? username : "");
			save(CookiesManager.COOKIE_PASSWORD, rememberMe ? password : "");
		} else {
			CookiesManager.save(CookiesManager.COOKIE_SAVELOGIN, "false");
			CookiesManager.save(CookiesManager.COOKIE_USER, "");
			CookiesManager.save(CookiesManager.COOKIE_PASSWORD, "");
		}
	}
}