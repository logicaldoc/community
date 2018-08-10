package com.logicaldoc.core.security;

import junit.framework.Assert;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Test case for the <code>SessionManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6
 */
public class SessionManagerTest extends AbstractCoreTCase {

	@Test
	public void testNewSession() {
		SessionManager sm = SessionManager.get();
		sm.clear();
		Session session1 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session1);
		Session session2 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session2);
		Assert.assertFalse(session1.equals(session2));
		Assert.assertEquals(2, sm.getSessions().size());
	}

	@Test
	public void testKill() {
		SessionManager sm = SessionManager.get();
		sm.clear();
		Session session1 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session1);
		Session session2 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session2);
		Assert.assertFalse(session1.equals(session2));
		Assert.assertEquals(2, sm.getSessions().size());

		sm.kill(session1.getSid());
		Assert.assertTrue(sm.isOpen(session2.getSid()));
		Assert.assertTrue(!sm.isOpen(session1.getSid()));
		Assert.assertEquals(2, sm.getSessions().size());
	}

	@Test
	public void testTimeout() {
		ContextProperties conf = Context.get().getProperties();
		int timeout=1;
		conf.setProperty("default.session.timeout", ""+timeout);

		SessionManager sm = SessionManager.get();
		sm.clear();
		Session session1 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session1);

		try {
			Thread.sleep(1000*60*timeout);
		} catch (InterruptedException e) {
		}

		Assert.assertFalse(sm.isOpen(session1.getSid()));
	}
}
