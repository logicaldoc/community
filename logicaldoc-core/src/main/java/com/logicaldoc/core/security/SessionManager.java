package com.logicaldoc.core.security;

import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.core.security.authentication.AuthenticationChain;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDDeferredSecurityContext;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Repository of all current user sessions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6
 */
@Component("sessionManager")
public class SessionManager extends ConcurrentHashMap<String, Session> {

	public static final String COOKIE_SID = "ldoc-sid";

	public static final String PARAM_SID = "sid";

	public static final String HEADER_APIKEY = "X-API-KEY";

	private static final Logger log = LoggerFactory.getLogger(SessionManager.class);

	private static final long serialVersionUID = 1L;

	// The maximum number of closed session maintained in memory
	private static final int MAX_CLOSED_SESSIONS = 50;

	@Resource(name = "authenticationChain")
	protected transient AuthenticationChain authenticationChain;

	@Resource(name = "sessionDAO")
	protected transient SessionDAO sessionDao;

	@Resource(name = "apiKeyDAO")
	protected transient ApiKeyDAO apiKeyDao;

	@Resource(name = "userDAO")
	protected transient UserDAO userDao;

	private transient SessionTimeoutWatchDog timeoutWatchDog = new SessionTimeoutWatchDog();

	private transient List<SessionListener> listeners = new ArrayList<>();

	public SessionManager() {
		timeoutWatchDog.start();
		log.info("Starting the session timeout watchdog");
	}

	public static final SessionManager get() {
		return Context.get(SessionManager.class);
	}

	/**
	 * Creates a new session by authenticating the given user and stores it in
	 * the pool of opened sessions
	 * 
	 * @param username the username
	 * @param password the passowrd
	 * @param key the secret key
	 * @param request the current request
	 * 
	 * @return the session created after the successful login
	 *
	 * @throws AuthenticationException raised in case of failed login
	 */
	public synchronized Session newSession(String username, String password, String key, HttpServletRequest request)
			throws AuthenticationException {
		return newSession(username, password, key, buildClient(request));
	}

	/**
	 * Creates a new session by authenticating the given user and stores it in
	 * the pool of opened sessions
	 * 
	 * @param username the username
	 * @param password the passowrd
	 * @param request the current request
	 * 
	 * @return the session created after the successful login
	 *
	 * @throws AuthenticationException raised in case of failed login
	 */
	public synchronized Session newSession(String username, String password, HttpServletRequest request)
			throws AuthenticationException {
		return newSession(username, password, buildClient(request));
	}

	/**
	 * Creates a new session by authenticating the given user and stores it in
	 * the pool of opened sessions
	 * 
	 * @param username the username
	 * @param password the passowrd
	 * @param client client informations
	 * 
	 * @return the session created after the successful login
	 *
	 * @throws AuthenticationException raised in case of failed login
	 */
	public synchronized Session newSession(String username, String password, Client client)
			throws AuthenticationException {
		return newSession(username, password, null, client);
	}

	/**
	 * Creates a new session by authenticating the given user and stores it in
	 * the pool of opened sessions
	 * 
	 * @param username the username
	 * @param password the passowrd
	 * @param key the secret key
	 * @param client client informations
	 * 
	 * @return the session created after the successful login
	 *
	 * @throws AuthenticationException raised in case of failed login
	 */
	public synchronized Session newSession(String username, String password, String key, Client client)
			throws AuthenticationException {
		try {
			User user = authenticationChain.authenticate(username, password, key, client);
			if (user == null)
				return null;
			else {
				return createSession(user, key, client);
			}
		} catch (AuthenticationException e) {
			LoginThrottle.recordFailure(username, key, client, e);
			throw e;
		}
	}

	/**
	 * Creates a new session by authenticating through an API Key and stores it
	 * in the pool of opened sessions
	 * 
	 * @param apikey the API Key
	 * @param client client informations
	 * 
	 * @return the session created after the successful login
	 *
	 * @throws AuthenticationException raised in case of failed login
	 */
	public synchronized Session newSession(String apikey, Client client) throws AuthenticationException {
		try {
			User user = authenticationChain.authenticate("", "", apikey, client);
			if (user == null)
				return null;
			else
				return createSession(user, apikey, client);
		} catch (AuthenticationException e) {
			LoginThrottle.recordFailure(null, apikey, client, e);
			throw e;
		}
	}

	/**
	 * Creates a new session by authenticating through an API Key and stores it
	 * in the pool of opened sessions
	 * 
	 * @param apikey the API Key
	 * @param request the current request
	 * 
	 * @return the session created after the successful login
	 *
	 * @throws AuthenticationException raised in case of failed login
	 */
	public synchronized Session newSession(String apikey, HttpServletRequest request) throws AuthenticationException {
		return newSession(apikey, buildClient(request));
	}

	/**
	 * Creates a new session for the given user and stores it in the pool of
	 * opened sessions
	 * 
	 * @param username the username
	 * @param key the secret key
	 * @param client client informations
	 * 
	 * @return the session created after the successful login
	 */
	private synchronized Session createSession(User user, String key, Client client) {
		Session session = new Session(user, key, client);
		put(session.getSid(), session);
		log.warn("Created new session {} for user {}", session.getSid(), user.getUsername());
		cleanClosedSessions();
		storeSession(session);
		for (SessionListener listener : listeners)
			try {
				listener.onSessionCreated(session);
			} catch (Exception t) {
				log.warn(t.getMessage(), t);
			}

		return session;
	}

	private void storeSession(Session session) {
		try {
			if (session.getId() == 0L) {
				Session dbSession = new Session(session);
				sessionDao.store(dbSession);
				session.setId(dbSession.getId());
			} else {
				Session dbSession = sessionDao.findById(session.getId());
				if (dbSession != null) {
					dbSession.setDeleted(session.getDeleted());
					dbSession.setLastRenew(session.getLastRenew());
					dbSession.setStatus(session.getStatus());
					dbSession.setFinished(session.getFinished());
					sessionDao.store(dbSession);
				}
			}
		} catch (org.springframework.orm.hibernate5.HibernateOptimisticLockingFailureException fe1) {
			// May happen, forget it.
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}
	}

	/**
	 * Kills an existing session
	 * 
	 * @param sid identifier of the session to kill
	 */
	public void kill(String sid) {
		Session session = get(sid);
		if (session != null) {
			session.setClosed();
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			if (log.isWarnEnabled())
				log.warn("Killed session {} of user {} started at {}", sid, session.getUsername(),
						df.format(session.getCreation()));
			storeSession(session);
			for (SessionListener listener : listeners)
				try {
					listener.onSessionClosed(sid);
				} catch (Exception t) {
					log.warn(t.getMessage(), t);
				}
		} else {
			// Perhaps the session was not in memory but just on DB
			try {
				sessionDao.jdbcUpdate("update ld_session set ld_status=" + Session.STATUS_CLOSED + " where ld_sid='"
						+ SqlUtil.doubleQuotes(sid) + "'");
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	@Override
	public Session remove(Object sid) {
		kill((String) sid);

		try {
			sessionDao.delete(get(sid).getId());
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}

		return super.remove(sid);
	}

	/**
	 * Renews an opened session
	 * 
	 * @param sid The session to be renewed
	 */
	public void renew(String sid) {
		if (isOpen(sid)) {
			Session session = get(sid);
			if (session.isTimedOut()) {
				session.setExpired();
			} else {
				session.setLastRenew(new Date());
			}
		}
	}

	public int getStatus(String sid) {
		Session session = get(sid);
		if (session == null)
			return -1;
		if (session.getStatus() == Session.STATUS_OPEN && session.isTimedOut()) {
			session.setExpired();
			storeSession(session);
		}
		return session.getStatus();
	}

	/**
	 * Checks if a session is valid or not. A valid session is a one that exists
	 * and is in state OPEN
	 * 
	 * @param sid The session identifier
	 * 
	 * @return true only if the session exists and is OPEN
	 */
	public boolean isOpen(String sid) {
		if (sid == null)
			return false;
		return Session.STATUS_OPEN == getStatus(sid);
	}

	@Override
	public Session get(Object sid) {
		if (sid == null)
			return null;
		return super.get(sid);
	}

	/**
	 * Gets the session of the given client
	 * 
	 * @param clientId identifier of the client
	 * 
	 * @return the session
	 */
	public Session getByClientId(String clientId) {
		if (clientId == null)
			return null;

		for (Session session : getSessions()) {
			if (session.getClient() != null && clientId.equals(session.getClient().getId()))
				return session;
		}

		return null;
	}

	/**
	 * Gets the session with the specified dictionary value
	 * 
	 * @param key identifier of the value in the dictionary
	 * @param value the value to match
	 * 
	 * @return the session
	 */
	public Session getByDictionaryValue(String key, Object value) {
		return getSessions().stream().filter(s -> value.equals(s.getDictionary().get(key))).findFirst().orElse(null);
	}

	/**
	 * Counts the total number of opened sessions
	 * 
	 * @return number of opened sessions
	 */
	public int countOpened() {
		return sessionDao.countSessions(null, Session.STATUS_OPEN);
	}

	/**
	 * Counts the total number of opened sessions per tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return number of opened sessions
	 */
	public int countOpened(long tenantId) {
		return sessionDao.countSessions(tenantId, Session.STATUS_OPEN);
	}

	/**
	 * Returns the list of sessions of the current node ordered by ascending
	 * status and creation date.
	 * 
	 * @return list of sessions
	 */
	public List<Session> getSessions() {
		List<Session> sessions = new ArrayList<>(values());
		Collections.sort(sessions);
		return sessions;
	}

	/**
	 * Clean method that removes all closed sessions that exceed the number of
	 * {@value #MAX_CLOSED_SESSIONS}
	 */
	private void cleanClosedSessions() {
		List<String> garbage = new ArrayList<>();
		int counter = 0;
		for (Session session : getSessions()) {
			if (getStatus(session.getSid()) != Session.STATUS_OPEN)
				counter++;
			if (counter > MAX_CLOSED_SESSIONS)
				garbage.add(session.getSid());
		}
		for (String sid : garbage) {
			remove(sid);
		}
	}

	/**
	 * Gets the Session with the identifier returned by
	 * {@link #getSessionId(HttpServletRequest)}
	 * 
	 * @param request the HTTP request
	 * 
	 * @return the found session, can be null
	 */
	public Session getSession(HttpServletRequest request) {
		String sid = getSessionId(request);
		if (sid == null)
			return null;
		if (isOpen(sid))
			return get(sid);
		return null;
	}

	/**
	 * Gets the Session ID specification from the current request following this
	 * lookup strategy:
	 * <ol>
	 * <li>Request parameter <code>PARAM_SID</code></li>
	 * <li>Request header <code>PARAM_SID</code></li>
	 * <li>Request attribute <code>PARAM_SID</code></li>
	 * <li>Session attribute <code>PARAM_SID</code></li>
	 * <li>Cookie <code>COOKIE_SID</code></li>
	 * <li>Header <code>X-API-KEY</code></li>
	 * <li>Spring SecurityContextHolder</li>
	 * <li>Client ID</li>
	 * </ol>
	 * 
	 * @param request The current request to inspect
	 * 
	 * @return The SID if any
	 */
	public String getSessionId(HttpServletRequest request) {
		String sid = getSessionIdFromRequest(request);
		if (sid != null)
			return sid;

		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		if (auth instanceof LDAuthenticationToken ldAuthenticationToken)
			return ldAuthenticationToken.getSid();

		return getSessionIdFromClient(request);
	}

	private String getSessionIdFromClient(HttpServletRequest request) {
		if (request == null || !Context.get().getProperties().getBoolean("security.useclientid", false))
			return null;

		Client client = buildClient(request);
		Session session = getByClientId(client.getId());

		/*
		 * In case of ClienID match, we must check the session provides Basic
		 * Authentication and refers to the same username
		 */
		if (session != null && isOpen(session.getSid()) && session.getUsername().equals(client.getUsername())) {
			String[] credentials = getBasicCredentials(request);
			if (credentials.length == 2) {
				try {
					/*
					 * In case the current user has defined a password, also
					 * check it matches with the basic authentication
					 */
					final String sessionUserPassword = session.getUser().getPassword();
					if (StringUtils.isEmpty(sessionUserPassword)
							|| CryptUtil.encryptSHA256(credentials[1]).equals(sessionUserPassword))
						return session.getSid();
				} catch (NoSuchAlgorithmException e) {
					log.error("Unable to check credentials", e);
				}
			}
		}

		return null;
	}

	private String getSessionIdFromRequest(HttpServletRequest request) {
		if (request == null)
			return null;

		String sid = null;
		if (StringUtils.isNotEmpty(request.getParameter(PARAM_SID))
				&& Context.get().getProperties().getBoolean("security.acceptsid", false)) {
			log.debug("Got SID in request {}", PARAM_SID);
			sid = request.getParameter(PARAM_SID);
		} else if (StringUtils.isNotEmpty(request.getHeader(PARAM_SID))) {
			log.debug("Got SID in request header {}", PARAM_SID);
			sid = request.getHeader(PARAM_SID);
		} else if (request.getAttribute(PARAM_SID) != null
				&& StringUtils.isNotEmpty((String) request.getAttribute(PARAM_SID))) {
			log.debug("Got SID in request attribute {}", PARAM_SID);
			sid = (String) request.getAttribute(PARAM_SID);
		} else if (request.getSession(true).getAttribute(PARAM_SID) != null
				&& StringUtils.isNotEmpty((String) request.getSession(true).getAttribute(PARAM_SID))) {
			log.debug("Got SID in session attribute {}", PARAM_SID);
			sid = (String) request.getSession(true).getAttribute(PARAM_SID);
		} else {
			sid = getSessionIdFromCookie(request);
		}

		if (StringUtils.isEmpty(sid))
			sid = getSessionFromApiKey(request);

		return sid;
	}

	private String getSessionFromApiKey(HttpServletRequest request) {
		String sid = null;
		try {
			if (StringUtils.isNotEmpty(request.getHeader(HEADER_APIKEY))) {
				log.debug("Found API Key in header {}", HEADER_APIKEY);
				String apiKey = CryptUtil.encryptSHA256(request.getHeader(HEADER_APIKEY));
				sid = getSessions().stream().filter(s -> apiKey.equals(s.getKey()) && s.isOpen())
						.map(Session::getSid).findFirst().orElse(null);
				if (StringUtils.isNotEmpty(sid))
					log.debug("Found SID bound to API Key in header {}", HEADER_APIKEY);
			}
		} catch (NoSuchAlgorithmException e) {
			log.warn(e.getMessage(), e);
		}

		return sid;
	}

	private String getSessionIdFromCookie(HttpServletRequest request) {
		Cookie[] cookies = request.getCookies();
		if (cookies != null)
			for (Cookie cookie : cookies) {
				if (COOKIE_SID.equals(cookie.getName())) {
					log.debug("Got SID in cookie {}", COOKIE_SID);
					return cookie.getValue();
				}
			}
		return null;
	}

	/**
	 * Saves the session identifier in the request and session attribute
	 * <code>PARAM_SID</code> and Cookie <code>COOKIE_SID</code>
	 * 
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param sid identifier of the session
	 */
	public void saveSid(HttpServletRequest request, HttpServletResponse response, String sid) {
		request.setAttribute(PARAM_SID, sid);
		request.getSession(true).setAttribute(PARAM_SID, sid);

		log.debug("Saved sid {} in session {}", sid, request.getSession(true).getId());
	}

	/**
	 * Removes the Sid from the http request
	 * 
	 * @param request the HTTP request
	 */
	public void removeSid(HttpServletRequest request) {
		if (request != null) {
			request.removeAttribute(PARAM_SID);
			if (request.getSession(false) != null) {
				log.debug("remove sid {} from session {}", request.getSession(false).getAttribute(PARAM_SID),
						request.getSession(true).getId());
				request.getSession(false).removeAttribute(PARAM_SID);
			}
		}

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		if (authentication != null)
			SecurityContextHolder.getContext().setAuthentication(null);
	}

	/**
	 * Retrieves the session ID of the current thread execution
	 * 
	 * @return the identifier of the session
	 */
	public static String getCurrentSid() {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		if (auth instanceof LDAuthenticationToken ldAuthenticationToken)
			return ldAuthenticationToken.getSid();
		else
			return null;
	}

	public HttpSession getServletSession(String sid) {
		if (sid == null)
			return null;
		return LDDeferredSecurityContext.getServletSession(sid);
	}

	/**
	 * Create a client identified using a concatenation of Basic authentication
	 * credentials and remote IP.
	 * 
	 * @param request The request to process
	 * 
	 * @return The client
	 */
	public Client buildClient(HttpServletRequest request) {
		Client client = new Client(request);

		/**
		 * We extract the username used by the user from the basic credentials.
		 * This may differ from the real username in case the login.ignorecase
		 * flag is activates
		 */
		String[] credentials = getBasicCredentials(request);
		if (credentials.length > 0 && credentials[0] != null)
			client.setUsername(credentials[0]);

		String authorization = request.getHeader("Authorization");
		String apiKey = request.getHeader(HEADER_APIKEY);

		client.setId(String.format("%s-%s-%s-%s-%s", StringUtils.defaultString(client.getUsername(), "0"),
				StringUtils.isNotEmpty(authorization) ? Integer.toString(authorization.hashCode()) : "0",
				StringUtils.isNotEmpty(apiKey) ? Integer.toString(apiKey.hashCode()) : "0", request.getRemoteAddr(),
				StringUtils.defaultString(request.getHeader("user-agent"))));
		return client;
	}

	private static String[] getBasicCredentials(HttpServletRequest req) {
		final String authorization = req.getHeader("Authorization");
		if (authorization != null && authorization.startsWith("Basic")) {
			// Authorization: Basic base64credentials
			String base64Credentials = authorization.substring("Basic".length()).trim();
			String credentials = new String(Base64.getDecoder().decode(base64Credentials), StandardCharsets.UTF_8);

			// credentials = username:password
			return credentials.split(":", 2);
		} else
			return new String[0];
	}

	@PreDestroy
	public void destroy() {
		log.info("Stopping the session timeout watchdog");
		timeoutWatchDog.finish();

		for (Session session : getSessions()) {
			try {
				SessionManager.get().kill(session.getSid());
			} catch (Exception t) {
				// Nothing to do
			}
		}
		clear();

		if (timeoutWatchDog.isAlive()) {
			try {
				timeoutWatchDog.interrupt();
			} catch (Exception t) {
				// Nothing to do
			}
			log.info("Session timeout watch dog killed");
		}
	}

	/**
	 * Each minute iterates over the sessions killing the expired ones
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 7.5.3
	 */
	class SessionTimeoutWatchDog extends Thread {
		boolean active = true;

		private SessionTimeoutWatchDog() {
			setDaemon(true);
			setName("SessionTimeoutWatchDog");
		}

		@Override
		public void run() {
			while (active) {
				synchronized (this) {
					try {
						wait(1000 * 60L);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
				}

				for (Session session : SessionManager.this.getSessions()) {
					if (session.isOpen() && session.isTimedOut()) {
						session.setExpired();
						storeSession(session);
					}
				}
			}
		}

		public void finish() {
			this.active = false;
		}

		public boolean isActive() {
			return active;
		}
	}

	public synchronized void addListener(SessionListener listener) {
		if (!listeners.contains(listener))
			listeners.add(listener);
	}

	public synchronized void removeListener(SessionListener listener) {
		listeners.remove(listener);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((listeners == null) ? 0 : listeners.hashCode());
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
		SessionManager other = (SessionManager) obj;
		if (listeners == null) {
			if (other.listeners != null)
				return false;
		} else if (!listeners.equals(other.listeners))
			return false;
		return true;
	}
}