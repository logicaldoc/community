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
	private GroupDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		dao = GroupDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		assertNotNull(dao.findById(10));

		dao.delete(10);
		assertNull(dao.findById(10));

		// Try to delete undeletable group
		try {
			dao.delete(1);
			fail("Group admin cannot be deleted");
		} catch (PersistenceException e) {
			// we expect an exception here
		}
		assertNotNull(dao.findById(1L));
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Group group = dao.findByName("admin", 1);
		assertNotNull(group);
		assertEquals("admin", group.getName());

		// Try with unexisting name
		group = dao.findByName("xxxx", 1);
		assertNull(group);

		// Try with unexisting tenant
		group = dao.findByName("admin", 99);
		assertNull(group);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Group group = dao.findById(1);
		assertNotNull(group);
		assertEquals("admin", group.getName());

		// Try with unexisting id
		group = dao.findById(999);
		assertNull(group);
	}

	@Test
	public void testFindAllGroupNames() throws PersistenceException {
		Collection<String> groupNames = dao.findAllGroupNames(1);
		assertNotNull(groupNames);
		assertFalse(groupNames.isEmpty());
		assertTrue(groupNames.contains("admin"));
		assertTrue(groupNames.contains("testGroup"));
	}

	@Test
	public void testStore() throws PersistenceException {
		assertNull(dao.findByName("LogicalObjects", 1));

		Group group = new Group();
		group.setName("LogicalObjects");
		group.setDescription("Test group for store method");

		dao.store(group);
		assertNotNull(dao.findByName("LogicalObjects", 1));

		Group group2 = dao.findByName("LogicalObjects", 1);
		assertEquals(group, group2);
	}

	@Test
	public void testInsert() throws PersistenceException {
		assertNull(dao.findByName("parentNone", 1));

		Group group = new Group();
		group.setName("parentNone");
		group.setDescription("Test group for insert method parent = none");

		dao.insert(group, 90);
		assertNotNull(dao.findByName("parentNone", 1));

		// Test with parentGroup Not Empty
		assertNull(dao.findByName("parentNotEmpty", 1));

		group = new Group();
		group.setName("parentNotEmpty");
		group.setDescription("Test group for insertX method parentGroup Not Empty");

		dao.insert(group, 90);
		assertNotNull(dao.findByName("parentNotEmpty", 1));
	}

	@Test
	public void testInheritACLs() throws PersistenceException {
		Group group = new Group();
		group.setName("parentNone");
		group.setDescription("Test group for insert method parent = none");

		dao.insert(group, 0);

		MenuDAO menuDao = MenuDAO.get();

		Menu menu = menuDao.findById(5L);
		menuDao.initialize(menu);
		assertNull(menu.getAccessControlEntry(group.getId()));

		dao.inheritACLs(group, 2);
		menu = menuDao.findById(5L);
		menuDao.initialize(menu);
		assertTrue(menu.getAccessControlEntry(group.getId()).isRead());

		menu = menuDao.findById(2L);
		menuDao.initialize(menu);
		assertNull(menu.getAccessControlEntry(group.getId()));
	}
}