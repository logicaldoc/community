package com.logicaldoc.core.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.security.core.context.SecurityContextHolder;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDDeferredSecurityContext;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Test case for the <code>SessionManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6
 */
@RunWith(MockitoJUnitRunner.class)
public class SessionManagerTest extends AbstractCoreTestCase implements SessionListener {

	// instance under test
	private SessionManager testSubject;

	@Mock
	private HttpServletRequest request;

	@Mock
	private HttpSession httpSession;

	@Mock
	private HttpServletResponse response;

	@Mock
	private Client client;

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		testSubject = SessionManager.get();
		testSubject.addListener(this);

		when(client.getId()).thenReturn("testid");
		when(request.getHeader("Authorization")).thenReturn("Basic YWRtaW46YWRtaW4=");
		when(request.getSession(true)).thenReturn(httpSession);

		SecurityContextHolder.getContext().setAuthentication(null);
	}

	@After
	@Override
	public void tearDown() throws IOException {
		testSubject.removeListener(this);
		testSubject.destroy();
		super.tearDown();
	}

	@Test
	public void testNewSession() throws AuthenticationException {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		Session session4 = testSubject.newSession(apiKey.getDecodedKey(), (Client) null);
		assertNotNull(session4);
	}

	@Test
	public void testKill() {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		testSubject.kill(session1.getSid());
		assertTrue(testSubject.isOpen(session2.getSid()));
		assertTrue(!testSubject.isOpen(session1.getSid()));
		assertEquals(2, testSubject.getSessions().size());
	}

	@Test
	public void testRemove() {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		testSubject.remove(session1.getSid());
		assertTrue(testSubject.isOpen(session2.getSid()));
		assertEquals(1, testSubject.getSessions().size());
	}

	@Test
	public void testRemoveSid() {
		testSubject.clear();
		Session session = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session);

		when(request.getSession(false)).thenReturn(httpSession);

		LDDeferredSecurityContext.bindServletSession(session.getSid(), httpSession);

		LDAuthenticationToken authentication = new LDAuthenticationToken("admin");
		authentication.setSid(session.getSid());
		SecurityContextHolder.getContext().setAuthentication(authentication);

		testSubject.removeSid(request);
		assertNull(testSubject.getSessionId(request));
	}

	@Test
	public void testBuildClient() {
		Client clnt = testSubject.buildClient(request);
		assertEquals("admin", clnt.getUsername());
	}

	@Test
	public void testRenew() {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		testSubject.renew(session1.getSid());
		assertTrue(testSubject.isOpen(session1.getSid()));
		assertEquals(2, testSubject.getSessions().size());
	}

	@Test
	public void testCount() {
		testSubject.clear();
		assertEquals(1, testSubject.countOpened(Tenant.DEFAULT_ID));

		Session session1 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session1);
		assertTrue(testSubject.isOpen(session1.getSid()));

		assertEquals(2, testSubject.countOpened(Tenant.DEFAULT_ID));
		assertEquals(2, testSubject.countOpened());
	}

	@Test
	public void testGetByClientId() {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", client);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		assertEquals(session1, testSubject.getByClientId("testid"));
	}

	@Test
	public void testTimeout() {
		ContextProperties conf = Context.get().getConfig();
		int timeout = 1;
		conf.setProperty("default.session.timeout", String.valueOf(timeout));

		testSubject.clear();

		Session session1 = testSubject.newSession("admin", "admin", (Client) null);
		assertNotNull(session1);

		Date expiredDate = new Date(System.currentTimeMillis() - (timeout + 1) * 60 * 1000L);
		session1.setLastRenew(expiredDate);

		assertFalse(testSubject.isOpen(session1.getSid()));
	}

	@Test
	public void getSesssion() {
		testSubject.clear();
		Session session = testSubject.newSession("admin", "admin", (Client) null);
		when(request.getParameter(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		when(httpSession.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		assertEquals(session, testSubject.getSession(request));

		when(request.getSession(false)).thenReturn(httpSession);
		when(request.getSession(false).getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		assertEquals(session, testSubject.getSession(request));

		when(request.getSession(false).getAttribute(SessionManager.PARAM_SID)).thenReturn(null);
		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		assertEquals(session, testSubject.getSession(request));

		when(request.getParameter(SessionManager.PARAM_SID)).thenReturn(null);
		when(request.getSession(false).getAttribute(SessionManager.PARAM_SID)).thenReturn(null);
		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(null);
		Cookie cookie = new Cookie(SessionManager.COOKIE_SID, session.getSid());
		when(request.getCookies()).thenReturn(new Cookie[] { cookie });
		assertEquals(session, testSubject.getSession(request));
	}

	@Test
	public void saveSid() {
		Session session = testSubject.newSession("admin", "admin", (Client) null);
		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());

		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		testSubject.saveSid(request, response, session.getSid());
		assertEquals(session.getSid(), testSubject.getSessionId(request));
	}

	@Test
	public void removeSid() {
		Session session = testSubject.newSession("admin", "admin", (Client) null);
		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());

		testSubject.saveSid(request, response, session.getSid());
		assertEquals(session.getSid(), testSubject.getSessionId(request));

		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		testSubject.remove(session.getSid());
		assertNull(testSubject.getSession(request));
	}

	@Test
	public void testCurrentSid() {
		assertNull(SessionManager.getCurrentSid());

		Session session = testSubject.newSession("admin", "admin", (Client) null);

		LDAuthenticationToken authentication = new LDAuthenticationToken("admin");
		authentication.setSid(session.getSid());
		SecurityContextHolder.getContext().setAuthentication(authentication);

		assertEquals(session.getSid(), SessionManager.getCurrentSid());
	}

	@Override
	public void onSessionCreated(Session session) {
		// Do nothing
	}

	@Override
	public void onSessionClosed(Object sid) {
		// Do nothing
	}
}