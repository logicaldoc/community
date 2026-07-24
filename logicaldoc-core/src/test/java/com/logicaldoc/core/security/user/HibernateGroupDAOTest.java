package com.logicaldoc.core.security.user;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateGroupDAOTest</code>
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class HibernateGroupDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private GroupDAO testSubject;

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = GroupDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		assertNotNull(testSubject.findById(10L));

		testSubject.delete(10L);
		assertNull(testSubject.findById(10L));

		// Try to delete undeletable group
		try {
			testSubject.delete(1);
			fail("Group admin cannot be deleted");
		} catch (PersistenceException e) {
			// we expect an exception here
		}
		assertNotNull(testSubject.findById(Group.GROUPID_ADMIN));
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Group group = testSubject.findByName("admin", Group.GROUPID_ADMIN);
		assertNotNull(group);
		assertEquals("admin", group.getName());

		// Try with unexisting name
		group = testSubject.findByName("xxxx", Group.GROUPID_ADMIN);
		assertNull(group);

		// Try with unexisting tenant
		group = testSubject.findByName("admin", 99L);
		assertNull(group);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Group group = testSubject.findById(Group.GROUPID_ADMIN, true);
		assertNotNull(group);
		assertEquals("admin", group.getName());
        assertEquals(2, group.getUsers().size());
		
		
		// Try with unexisting id
		group = testSubject.findById(999L);
		assertNull(group);
	}

	@Test
	public void testFindAllGroupNames() throws PersistenceException {
		Collection<String> groupNames = testSubject.findAllGroupNames(1);
		assertNotNull(groupNames);
		assertFalse(groupNames.isEmpty());
		assertTrue(groupNames.contains("admin"));
		assertTrue(groupNames.contains("testGroup"));
	}

	@Test
	public void testStore() throws PersistenceException {
		assertNull(testSubject.findByName("LogicalObjects", Tenant.DEFAULT_ID));

		Group group = new Group();
		group.setName("LogicalObjects");
		group.setDescription("Test group for store method");

		testSubject.store(group);
		assertNotNull(testSubject.findByName("LogicalObjects", Tenant.DEFAULT_ID));

		Group group2 = testSubject.findByName("LogicalObjects", Tenant.DEFAULT_ID);
		assertEquals(group, group2);
	}

	@Test
	public void testInsert() throws PersistenceException {
		assertNull(testSubject.findByName("parentNone", 1));

		Group group = new Group();
		group.setName("parentNone");
		group.setDescription("Test group for insert method parent = none");

		testSubject.insert(group, 90);
		assertNotNull(testSubject.findByName("parentNone", Tenant.DEFAULT_ID));

		// Test with parentGroup Not Empty
		assertNull(testSubject.findByName("parentNotEmpty", Tenant.DEFAULT_ID));

		group = new Group();
		group.setName("parentNotEmpty");
		group.setDescription("Test group for insertX method parentGroup Not Empty");

		testSubject.insert(group, 90);
		assertNotNull(testSubject.findByName("parentNotEmpty", 1));
	}

	@Test
	public void testInheritACLs() throws PersistenceException {
		Group group = new Group();
		group.setName("parentNone");
		group.setDescription("Test group for insert method parent = none");

		testSubject.insert(group, 0);

		MenuDAO menuDao = MenuDAO.get();

		Menu menu = menuDao.findById(5L, true);
		assertNull(menu.getAccessControlEntry(group.getId()));

		testSubject.inheritACLs(group, 2L);
		menu = menuDao.findById(5L, true);
		assertTrue(menu.getAccessControlEntry(group.getId()).isRead());

		menu = menuDao.findById(2L, true);
		assertNull(menu.getAccessControlEntry(group.getId()));
	}
}