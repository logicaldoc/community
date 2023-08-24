package com.logicaldoc.web.service;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.web.AbstractWebappTestCase;

import junit.framework.Assert;

public class SecurityServiceImplTest extends AbstractWebappTestCase {

	// Instance under test
	private SecurityServiceImpl service = new SecurityServiceImpl();

	private UserDAO userDAO;

	private GroupDAO groupDAO;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		userDAO = (UserDAO) context.getBean("UserDAO");
		groupDAO = (GroupDAO) context.getBean("GroupDAO");
	}

	@Test
	public void testLogout() {
		Assert.assertEquals("admin", guiSession.getUser().getUsername());
		Assert.assertEquals(1, guiSession.getUser().getId());
		int sessions = SessionManager.get().countOpened();
		service.logout();

		Assert.assertEquals(sessions - 1, SessionManager.get().countOpened());
	}

	@Test
	public void testChangePassword() {
		Assert.assertEquals("0", service.changePassword(1L, 1L, "admin", "TBDcy@u<QOR;6}l", false).getCode());
		Assert.assertEquals("0",
				service.changePassword(1L, 1L, "TBDcy@u<QOR;6}l", "TBDcy@u<QOR;6}l;", false).getCode());
		Assert.assertEquals("0",
				service.changePassword(1L, 1L, "TBDcy@u<QOR;6}l;", "TBDcy@u<QOR;6}l-", false).getCode());
		;
	}

	@Test
	public void testAddUserToGroup() throws ServerException, PersistenceException {
		User test = userDAO.findByUsername("test");
		Assert.assertNotNull(test);
		Group group = groupDAO.findByName("author", Tenant.DEFAULT_ID);
		Assert.assertNotNull(group);
		service.addUserToGroup(group.getId(), test.getId());
		User user = userDAO.findByUsername("test");
		Assert.assertTrue(user.getGroups().contains(group));

		group = groupDAO.findByName("guest", Tenant.DEFAULT_ID);
		Assert.assertNotNull(group);
		service.addUserToGroup(group.getId(), test.getId());
		user = userDAO.findByUsername("test");
		Assert.assertTrue(user.getGroups().contains(group));
	}

	@Test
	public void testDeleteGroup() throws ServerException, PersistenceException {
		Assert.assertNotNull(groupDAO.findById(10));
		service.deleteGroup(10);
		Assert.assertNull(groupDAO.findById(10));

		// Delete a non-deleteable group
		Assert.assertNotNull(groupDAO.findById(1));
		try {
			service.deleteGroup(1);
			Assert.fail("Admin group has been deleted");
		} catch (Exception e) {
			// We expect an exception here
		}
		Assert.assertNotNull(groupDAO.findById(1));
	}

	@Test
	public void testDeleteUser() throws ServerException, PersistenceException {
		User user = userDAO.findByUsername("author");
		Assert.assertEquals(2, user.getGroups().size());
		service.deleteUser(user.getId());
		user = userDAO.findByUsername("author");
		Assert.assertNull(user);
	}

	@Test
	public void testRemoveFromGroup() throws ServerException, PersistenceException {
		long[] users = new long[2];
		users[0] = 5;
		users[1] = 1;
		Group group = groupDAO.findByName("author", Tenant.DEFAULT_ID);
		service.removeFromGroup(group.getId(), users);
		User user = userDAO.findByUsername("test");
		Assert.assertFalse(user.getGroups().contains(group));
		user = userDAO.findByUsername("admin");
		Assert.assertFalse(user.getGroups().contains(group));
	}

	@Test
	public void testGetGroup() throws ServerException {
		GUIGroup group = service.getGroup(10);
		Assert.assertNotNull(group);
		Assert.assertEquals("testGroup", group.getName());

		// Try with unexisting id
		group = service.getGroup(999);
		Assert.assertNull(group);
	}

	@Test
	public void testGetUser() throws ServerException {
		GUIUser user = service.getUser(1);
		Assert.assertNotNull(user);
		Assert.assertEquals("admin", user.getUsername());
		Assert.assertEquals("admin@admin.net", user.getEmail());

		user = service.getUser(3);
		Assert.assertNotNull(user);
		Assert.assertEquals("sebastian", user.getUsername());
		Assert.assertEquals("seb_stein@gmx.de", user.getEmail());
		Assert.assertEquals("de", user.getLanguage());

		// Try with unexisting id
		user = service.getUser(9999);
		Assert.assertNull(user);
	}

	@Test
	public void testSaveGroup() throws ServerException {
		GUIGroup group = service.getGroup(10);

		group = service.saveGroup(group);
		Assert.assertNotNull(group);
		Assert.assertEquals("testGroup", group.getName());
	}

	@Test
	public void testSaveUser() throws ServerException {
		GUIUser user = service.getUser(1);

		user = service.saveUser(user, guiSession.getInfo());
		Assert.assertNotNull(user);
		Assert.assertEquals("admin", user.getUsername());
		Assert.assertEquals("admin@admin.net", user.getEmail());

		user = service.getUser(3);

		user = service.saveUser(user, guiSession.getInfo());
		Assert.assertNotNull(user);
		Assert.assertEquals("sebastian", user.getUsername());
		Assert.assertEquals("seb_stein@gmx.de", user.getEmail());
		Assert.assertEquals("de", user.getLanguage());
	}

	@Test
	public void testKill() {
		SessionManager sm = SessionManager.get();
		sm.clear();
		Session session1 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session1);
		Session session2 = sm.newSession("admin", "admin", null);
		Assert.assertNotNull(session2);
		Assert.assertNotSame(session1, session2);
		Assert.assertEquals(2, sm.getSessions().size());

		service.kill(session1.getSid());
		Assert.assertTrue(sm.isOpen(session2.getSid()));
		Assert.assertFalse(sm.isOpen(session1.getSid()));
		Assert.assertEquals(2, sm.getSessions().size());
	}

	@Test
	public void testSaveSettings() {
		GUISecuritySettings securitySettings = new GUISecuritySettings();
		securitySettings.setPwdExpiration(30);
		securitySettings.setPwdSize(6);
		securitySettings.setAnonymousKey("xxx");

		String notThrownTest = null;
		try {
			service.saveSettings(securitySettings);
			notThrownTest = "ok";
		} catch (Exception t) {
			t.printStackTrace();
			// Nothing to do
		}
		Assert.assertEquals("ok", notThrownTest);
	}
}