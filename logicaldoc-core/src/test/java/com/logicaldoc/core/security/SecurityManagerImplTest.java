package com.logicaldoc.core.security;

import java.util.ArrayList;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;

import junit.framework.Assert;

/**
 * Test case for the manager <code>SecurityManager</code>.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class SecurityManagerImplTest extends AbstractCoreTCase {
	// Instance under test
	private SecurityManager manager;

	private UserDAO userDAO;

	private GroupDAO groupDAO;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		manager = (SecurityManager) context.getBean("SecurityManager");
		userDAO = (UserDAO) context.getBean("UserDAO");
		groupDAO = (GroupDAO) context.getBean("GroupDAO");
	}

	@Test
	public void testAssignUsersToGroup() throws PersistenceException {
		ArrayList<User> users = new ArrayList<User>();
		users.add(userDAO.findByUsername("test"));
		users.add(userDAO.findByUsername("admin"));
		Group group = groupDAO.findByName("author", 1);
		for (User u : users) {
			u.addGroup(group);
			userDAO.store(u);
		}

		User user = userDAO.findByUsername("test");
		Assert.assertTrue(user.getGroups().contains(group));
		user = userDAO.findByUsername("admin");
		Assert.assertTrue(user.getGroups().contains(group));

		group = groupDAO.findByName("guest", 1);
		for (User u : users) {
			u.addGroup(group);
			userDAO.store(u);
		}
		user = userDAO.findByUsername("test");
		Assert.assertTrue(user.getGroups().contains(group));
		user = userDAO.findByUsername("admin");
		Assert.assertTrue(user.getGroups().contains(group));
	}

	@Test
	public void testRemoveAllUsersFromGroup() throws PersistenceException {
		// create a new group which extends author
		Group authorGroup = groupDAO.findByName("author", 1);

		Group editorGroup = new Group();
		editorGroup.setName("editors");
		editorGroup.setDescription("Group for editors which extends author group.");

		groupDAO.insert(editorGroup, authorGroup.getId());

		// create 4 new users
		// assign the newly created users to the editor group
		User user = new User();
		user.setUsername("test1");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		userDAO.store(user);
		user.addGroup(editorGroup);
		userDAO.store(user);

		user = new User();
		user.setUsername("test2");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		userDAO.store(user);
		user.addGroup(editorGroup);
		userDAO.store(user);

		user = new User();
		user.setUsername("test3");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		userDAO.store(user);
		user.addGroup(editorGroup);
		userDAO.store(user);

		user = new User();
		user.setUsername("test4");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		userDAO.store(user);
		user.addGroup(editorGroup);
		userDAO.store(user);

		// remove all users from the group editors
		groupDAO.initialize(editorGroup);
		for (User u : editorGroup.getUsers()) {
			u.removeGroup(editorGroup.getId());
			userDAO.store(u);
		}

		// check
		User userf = userDAO.findByUsername("test4");
		Assert.assertFalse(userf.getGroups().contains(editorGroup));
	}

	@Test
	public void testAssignUserToGroups() {
		User user = new User();
		user.setUsername("zzz");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");

		String notThrownTest = null;
		try {
			userDAO.store(user);
			notThrownTest = "ok";
		} catch (Throwable t) {
			// Nothing to do
		}
		Assert.assertNotNull(notThrownTest);
	}

	@Test
	public void testGetAllowedGroups() {
		Set<Group> groups = manager.getAllowedGroups(9);
		Assert.assertNotNull(groups);
		Assert.assertTrue(groups.contains(groupDAO.findByName("admin", 1)));
	}

	@Test
	public void testIsMemberOfLongLong() {
		Assert.assertTrue(manager.isMemberOf(1, 1));
		Assert.assertFalse(manager.isMemberOf(1, 1234));
		Assert.assertFalse(manager.isMemberOf(1, 2));
	}

	@Test
	public void testIsMemberOfLongString() {
		Assert.assertTrue(manager.isMemberOf(1, "admin"));
		Assert.assertFalse(manager.isMemberOf(1, "xyz"));
		Assert.assertFalse(manager.isMemberOf(1, "author"));
	}
}