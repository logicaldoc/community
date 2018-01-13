package com.logicaldoc.webservice.soap.endpoint;

import java.util.Arrays;
import java.util.List;

import junit.framework.Assert;

import org.junit.Test;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.webservice.AbstractWebServiceTestCase;
import com.logicaldoc.webservice.model.WSGroup;
import com.logicaldoc.webservice.model.WSUser;

/**
 * Test case for <code>SoapSecurityService</code>
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.1
 */
public class SoapSecurityServiceTest extends AbstractWebServiceTestCase {
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
		WSUser[] users = securityServiceImpl.listUsers("");
		Assert.assertNotNull(users);
		Assert.assertEquals(5, users.length);
		List<WSUser> usersList = Arrays.asList(users);
		Assert.assertEquals(1, usersList.get(0).getId());
		Assert.assertEquals(2, usersList.get(1).getId());
		Assert.assertEquals("boss", usersList.get(1).getUsername());
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
	}

	@Test
	public void testStoreGroup() throws Exception {
		WSGroup wsGroupTest = new WSGroup();
		wsGroupTest.setName("group test");
		wsGroupTest.setDescription("group test descr");

		Long groupId = securityServiceImpl.storeGroup("", wsGroupTest);
		Assert.assertNotNull(groupId);
		
		Group createdGroup = groupDao.findById(groupId);
		Assert.assertNotNull(createdGroup);
		Assert.assertEquals("group test", createdGroup.getName());
		Assert.assertEquals("group test descr", createdGroup.getDescription());
	}

	@Test
	public void testDeleteUser() throws Exception {
		User user = userDao.findById(2);
		Assert.assertNotNull(user);
		securityServiceImpl.deleteUser("", user.getId());
		user = userDao.findById(2);
		Assert.assertNull(user);
	}

	@Test
	public void testDeleteGroup() throws Exception {
		Group group = groupDao.findById(2);
		Assert.assertNotNull(group);
		securityServiceImpl.deleteGroup("", group.getId());
		group = groupDao.findById(2);
		Assert.assertNull(group);
	}

	@Test
	public void testChangePassword() throws Exception {
		WSUser newUser = new WSUser();
		newUser.setName("user test");
		newUser.setUsername("user");
		newUser.setFirstName("test");
		newUser.setEmail("user@acme.com");

		Long userId = securityServiceImpl.storeUser("", newUser);
		Assert.assertNotNull(userId);

		int changeResult = securityServiceImpl.changePassword("", userId, null, "newpassword");
		Assert.assertEquals(0, changeResult);
	}
}