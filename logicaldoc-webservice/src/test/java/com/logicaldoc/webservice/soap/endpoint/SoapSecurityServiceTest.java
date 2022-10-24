package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSGroup;
import com.logicaldoc.webservice.model.WSUser;

import junit.framework.Assert;

/**
 * Test case for <code>SoapSecurityService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapSecurityServiceTest extends AbstractWebserviceTestCase {
	
	private UserDAO userDao;

	private GroupDAO groupDao;

	// Instance under test
	private SoapSecurityService securityServiceImpl;

	@Override
	public void setUp() throws Exception {
		super.setUp();
		userDao = (UserDAO) context.getBean("UserDAO");
		groupDao = (GroupDAO) context.getBean("GroupDAO");

		// Make sure that this is a SoapSecurityService instance
		securityServiceImpl = new SoapSecurityService();
		securityServiceImpl.setValidateSession(false);
	}

	@Test
	public void testListUsers() throws Exception {
		WSUser[] users = securityServiceImpl.listUsers("", null);
		Assert.assertNotNull(users);
		Assert.assertEquals(5, users.length);
		List<WSUser> usersList = Arrays.asList(users);
		Assert.assertEquals(1, usersList.get(0).getId());
		Assert.assertEquals(2, usersList.get(1).getId());
		Assert.assertEquals("boss", usersList.get(1).getUsername());

		users = securityServiceImpl.listUsers("", "testGroup");
		Assert.assertNotNull(users);
		Assert.assertEquals(3, users.length);

		securityServiceImpl.setValidateSession(true);
		SessionManager sm = SessionManager.get();
		Session session1 = sm.newSession("author", "admin", null);

		users = securityServiceImpl.listUsers(session1.getSid(), "admin");

		Assert.assertNotNull(users);
		Assert.assertEquals(2, users.length);

		for (WSUser wsUser : users) {
//			System.err.println("Name: " + wsUser.getName());
//			System.err.println("Username: " + wsUser.getUsername());
//			System.err.println("Password: " + wsUser.getPassword());
//			System.err.println("Email: " + wsUser.getEmail());
//			System.err.println("Email2: " + wsUser.getEmail2());
//			System.err.println("Passwordmd4: " + wsUser.getPasswordmd4());

			Assert.assertNull(wsUser.getUsername());
			Assert.assertNull(wsUser.getEmail());
			Assert.assertNull(wsUser.getEmail2());
			Assert.assertTrue(wsUser.getPassword() == null || wsUser.getPassword().isEmpty());
			Assert.assertNull(wsUser.getPasswordmd4());
		}
		
		securityServiceImpl.setValidateSession(false);
	}

	@Test
	public void testListGroups() throws Exception {
		WSGroup[] groups = securityServiceImpl.listGroups("");
		Assert.assertNotNull(groups);
		Assert.assertEquals(6, groups.length);
		List<WSGroup> groupsList = Arrays.asList(groups);
		
		WSGroup dummy = new WSGroup();
		dummy.setId(1);
		Assert.assertTrue(groupsList.contains(dummy));		
		dummy.setId(2);
		Assert.assertTrue(groupsList.contains(dummy));
		Assert.assertEquals("Group of authors", groupsList.get(groupsList.indexOf(dummy)).getDescription());		
	}

	@Test
	public void testStoreUser() throws Exception {
		WSUser wsUserTest = new WSUser();
		wsUserTest.setName("user test");
		wsUserTest.setEmail("user@acme.com");
		wsUserTest.setUsername("user");
		wsUserTest.setFirstName("test");

		Long userId = securityServiceImpl.storeUser("", wsUserTest);
		Assert.assertNotNull(userId);

		User createdUser = userDao.findById(userId);
		Assert.assertNotNull(createdUser);
		Assert.assertEquals("user test", createdUser.getName());
		Assert.assertEquals("user@acme.com", createdUser.getEmail());
		
		// test store user with id != 0 (existing user)
		WSUser tuser = securityServiceImpl.getUser("", 5);
		Assert.assertNotNull(tuser);
		
		tuser.setFirstName("Taylor");
		tuser.setName("Taylor Swift");
		tuser.setStreet("Cornelia Street");
		tuser.setCountry("United States");
		tuser.setCity("New York");				
		tuser.setLanguage("en");
		
		userId = securityServiceImpl.storeUser("", tuser);
		Assert.assertNotNull(userId);
		Assert.assertEquals(5L, userId.longValue());
		
		tuser = securityServiceImpl.getUser("", 5);
		Assert.assertEquals("Taylor", tuser.getFirstName());
		Assert.assertEquals("Taylor Swift", tuser.getName());
		Assert.assertEquals("Cornelia Street", tuser.getStreet());
		Assert.assertEquals("United States", tuser.getCountry());
		Assert.assertEquals("New York", tuser.getCity());
		
		// try to store system user
		wsUserTest = new WSUser();
		wsUserTest.setId(-1010);
		wsUserTest.setName("Lavender Haze");
		wsUserTest.setEmail("l.haze@midnights.com");
		wsUserTest.setUsername("lavhaze");
		wsUserTest.setFirstName("Lavender");
		wsUserTest.setType(User.TYPE_SYSTEM);

		try {
			userId = securityServiceImpl.storeUser("", wsUserTest);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}		
	}

	@Test
	public void testStoreGroup() throws Exception {
		WSGroup wsGroupTest = new WSGroup();
		wsGroupTest.setName("group test");
		wsGroupTest.setDescription("group test descr");

		Long groupId = securityServiceImpl.storeGroup("", wsGroupTest);
		Assert.assertNotNull(groupId);
		Assert.assertTrue(groupId.longValue()!=0L);

		Group createdGroup = groupDao.findById(groupId);
		Assert.assertNotNull(createdGroup);
		Assert.assertEquals("group test", createdGroup.getName());
		Assert.assertEquals("group test descr", createdGroup.getDescription());

		// test store user with id != 0 (existing user)
		WSGroup sgroup = securityServiceImpl.getGroup("", groupId);
		
		// you can't store group with name null or empty 
		try {
			sgroup.setName(null);
			securityServiceImpl.storeGroup("", sgroup);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}

		sgroup = securityServiceImpl.getGroup("", -2);
		Assert.assertNotNull(sgroup);
		
		// you can't store group whose type is not default
		try {
			sgroup.setName("Midnights");
			securityServiceImpl.storeGroup("", sgroup);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}
		
		// Test updating an existing group 
		sgroup = securityServiceImpl.getGroup("", 10);
		Assert.assertNotNull(sgroup);
		Assert.assertEquals("testGroup", sgroup.getName());
		Assert.assertNotNull(sgroup.getUserIds());
		Assert.assertTrue(sgroup.getUserIds().length > 0);
		
		sgroup.setName("Midnights");
		long groupIdSt = securityServiceImpl.storeGroup("", sgroup);
		Assert.assertEquals(10, groupIdSt);
		
		WSGroup updatedGroup = securityServiceImpl.getGroup("", groupIdSt);
		Assert.assertNotNull(updatedGroup);	
		Assert.assertEquals(10, updatedGroup.getId());
		Assert.assertEquals("Midnights", updatedGroup.getName());
	}

	@Test
	public void testDeleteUser() throws Exception {
		
		User user = userDao.findById(2);
		Assert.assertNotNull(user);
		securityServiceImpl.deleteUser("", user.getId());
		user = userDao.findById(2);
		Assert.assertNull(user);
		
		// Try to delete a user that cannot be deleted
		try {
			securityServiceImpl.deleteUser("", 1);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}

		// Attempt to delete a system user
		try {
			securityServiceImpl.deleteUser("", -1010);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}		
	}

	@Test
	public void testDeleteGroup() throws Exception {
		Group group = groupDao.findById(2);
		Assert.assertNotNull(group);
		securityServiceImpl.deleteGroup("", group.getId());
		group = groupDao.findById(2);
		Assert.assertNull(group);
		
		// Try to delete a group that cannot be deleted
		try {
			securityServiceImpl.deleteGroup("", 1);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// Nothing to do
		}
		
		// Attempt to delete a user' group (not deletable)
		try {
			securityServiceImpl.deleteGroup("", -2);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// Nothing to do
		}		
	}

	@Test
	public void testChangePassword() throws Exception {
		WSUser newUser = new WSUser();
		newUser.setName("user test");
		newUser.setUsername("user");
		newUser.setFirstName("test");
		newUser.setEmail("user@acme.com");
		newUser.setPassword("(-xi%HT3y?r3'ux");

		Long userId = securityServiceImpl.storeUser("", newUser);
		Assert.assertNotNull(userId);

		int changeResult = securityServiceImpl.changePassword("", userId, "(-xi%HT3y?r3'ux", "t<(`oN]I{*2d(0");
		Assert.assertEquals(0, changeResult);
		
		// attempt to change the passwd using the previously used passwd (same passwd)
		changeResult = securityServiceImpl.changePassword("", userId, "(-xi%HT3y?r3'ux", "t<(`oN]I{*2d(0");
		Assert.assertEquals(1, changeResult);
		
		// attempt to change password of a non existent user
		changeResult = securityServiceImpl.changePassword("", 2110, "(-xi%HT3y?r3'ux", "t<(`oN]I{*2d(0");
		Assert.assertEquals(1, changeResult);
	}
	
	@Test
	public void testGetUser() throws Exception {
		WSUser user = securityServiceImpl.getUser("", 4);
		Assert.assertNotNull(user);
		Assert.assertEquals("Author", user.getName());
		Assert.assertEquals("author@acme.com", user.getEmail());
		
		// test for non-existent user
		user = securityServiceImpl.getUser("", 400);
		Assert.assertNull(user);
	}

	@Test
	public void testGetUserByUsername() throws Exception {
		WSUser user = securityServiceImpl.getUserByUsername("", "author");
		Assert.assertNotNull(user);
		Assert.assertEquals("Author", user.getName());
		Assert.assertEquals("author@acme.com", user.getEmail());
		
		// test for non-existent user
		user = securityServiceImpl.getUserByUsername("", "Midnights");
		Assert.assertNull(user);		
	}

	@Test
	public void testGetGroup() throws Exception {
		WSGroup group = securityServiceImpl.getGroup("", 2);
		Assert.assertNotNull(group);
		Assert.assertEquals("author", group.getName());
		
		// test for non-existent group
		group = securityServiceImpl.getGroup("", 200);
		Assert.assertNull(group);
	}	
}