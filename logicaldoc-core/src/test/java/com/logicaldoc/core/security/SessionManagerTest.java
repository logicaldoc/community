package com.logicaldoc.core.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.security.core.context.SecurityContextHolder;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDSecurityContextRepository;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Test case for the <code>SessionManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6
 */
@RunWith(MockitoJUnitRunner.class)
public class SessionManagerTest extends AbstractCoreTCase implements SessionListener {

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
	public void setUp() throws Exception {
		super.setUp();
		testSubject = SessionManager.get();
		testSubject.addListener(this);

		when(client.getId()).thenReturn("testid");
		when(request.getHeader("Authorization")).thenReturn("Basic YWRtaW46YWRtaW4=");
	}

	@After
	public void tearDown() throws Exception {
		testSubject.removeListener(this);
		testSubject.destroy();
		super.tearDown();
	}

	@Test
	public void testNewSession() throws AuthenticationException, PersistenceException {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
		Session session3 = testSubject.createSession(uDao.findById(1L), client);
		assertNotNull(session3);
		assertEquals(3, testSubject.getSessions().size());
	}

	@Test
	public void testKill() {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", null);
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
		Session session1 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", null);
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
		Session session = testSubject.newSession("admin", "admin", null);
		assertNotNull(session);
		
		when(request.getSession(false)).thenReturn(httpSession);
		
		LDSecurityContextRepository.bindServletSession(session.getSid(), httpSession);
		
		LDAuthenticationToken authentication=new LDAuthenticationToken("admin");
		authentication.setSid(session.getSid());
		SecurityContextHolder.getContext().setAuthentication(authentication);
		
		testSubject.removeSid(request);
		assertNull(testSubject.getSessionId(request));
	}

	@Test
	public void testBuildClient() {
		Client client=testSubject.buildClient(request);
		assertEquals("admin", client.getUsername());
	}

	@Test
	public void testRenew() {
		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session1);
		Session session2 = testSubject.newSession("admin", "admin", null);
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

		Session session1 = testSubject.newSession("admin", "admin", null);
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
		Session session2 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, testSubject.getSessions().size());

		assertEquals(session1, testSubject.getByClientId("testid"));
	}

	@Test
	public void testTimeout() {
		ContextProperties conf = Context.get().getProperties();
		int timeout = 1;
		conf.setProperty("default.session.timeout", "" + timeout);

		testSubject.clear();
		Session session1 = testSubject.newSession("admin", "admin", null);
		assertNotNull(session1);

		waiting(timeout);

		assertFalse(testSubject.isOpen(session1.getSid()));
	}

	@Test
	public void getSesssion() {
		testSubject.clear();
		Session session = testSubject.newSession("admin", "admin", null);
		when(request.getParameter(SessionManager.PARAM_SID)).thenReturn(session.getSid());
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
		Session session = testSubject.newSession("admin", "admin", null);
		when(request.getSession(false)).thenReturn(httpSession);
		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());

		when(request.getAttribute(SessionManager.PARAM_SID)).thenReturn(session.getSid());
		testSubject.saveSid(request, response, session.getSid());
		assertEquals(session.getSid(), testSubject.getSessionId(request));
	}

	@Test
	public void removeSid() {
		Session session = testSubject.newSession("admin", "admin", null);
		when(request.getSession(false)).thenReturn(httpSession);
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
		
		Session session = testSubject.newSession("admin", "admin", null);
		
		LDAuthenticationToken authentication=new LDAuthenticationToken("admin");
		authentication.setSid(session.getSid());
		SecurityContextHolder.getContext().setAuthentication(authentication);
		
		assertEquals(session.getSid(), SessionManager.getCurrentSid());
	}

	private void waiting(int timeout) {
		synchronized (this) {
			try {
				wait(1000 * 60 * timeout);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
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