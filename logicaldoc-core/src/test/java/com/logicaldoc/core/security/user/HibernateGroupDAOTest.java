package com.logicaldoc.core.security.user;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateGroupDAOTest</code>
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class HibernateGroupDAOTest extends AbstractCoreTestCase {

	protected static Logger log = LoggerFactory.getLogger(HibernateGroupDAOTest.class);

	// Instance under test
	private GroupDAO dao;

	private com.logicaldoc.core.security.SecurityManager manager;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateGroupDAO
		dao = (GroupDAO) context.getBean("GroupDAO");
		manager = (com.logicaldoc.core.security.SecurityManager) context.getBean("SecurityManager");
	}

	@Test
	public void testDelete() throws PersistenceException {
		Assert.assertNotNull(dao.findById(10));

		dao.delete(10);
		Assert.assertNull(dao.findById(10));

		// Try to delete undeletable group
		try {
			dao.delete(1);
			Assert.fail("Group admin cannot be deleted");
		} catch (PersistenceException e) {
			// we expect an exception here
		}
		Assert.assertNotNull(dao.findById(1L));
	}

	@Test
	public void testFindByName() {
		Group group = dao.findByName("admin", 1);
		Assert.assertNotNull(group);
		Assert.assertEquals("admin", group.getName());

		// Try with unexisting name
		group = dao.findByName("xxxx", 1);
		Assert.assertNull(group);

		// Try with unexisting tenant
		group = dao.findByName("admin", 99);
		Assert.assertNull(group);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Group group = dao.findById(1);
		Assert.assertNotNull(group);
		Assert.assertEquals("admin", group.getName());

		// Try with unexisting id
		group = dao.findById(999);
		Assert.assertNull(group);
	}

	@Test
	public void testFindAllGroupNames() {
		Collection<String> groupNames = dao.findAllGroupNames(1);
		Assert.assertNotNull(groupNames);
		Assert.assertFalse(groupNames.isEmpty());
		Assert.assertTrue(groupNames.contains("admin"));
		Assert.assertTrue(groupNames.contains("testGroup"));
	}

	@Test
	public void testStore() throws PersistenceException {
		Assert.assertNull(dao.findByName("LogicalObjects", 1));

		Group group = new Group();
		group.setName("LogicalObjects");
		group.setDescription("Test group for store method");

		dao.store(group);
		Assert.assertNotNull(dao.findByName("LogicalObjects", 1));
		
		Group group2 = dao.findByName("LogicalObjects", 1);
		Assert.assertEquals(group, group2);
	}

	@Test
	public void testInsert() {
		Assert.assertNull(dao.findByName("parentNone", 1));

		Group group = new Group();
		group.setName("parentNone");
		group.setDescription("Test group for insert method parent = none");

		Assert.assertTrue(dao.insert(group, 90));
		Assert.assertNotNull(dao.findByName("parentNone", 1));

		// Test with parentGroup Not Empty
		Assert.assertNull(dao.findByName("parentNotEmpty", 1));

		group = new Group();
		group.setName("parentNotEmpty");
		group.setDescription("Test group for insertX method parentGroup Not Empty");

		Assert.assertTrue(dao.insert(group, 90));
		Assert.assertNotNull(dao.findByName("parentNotEmpty", 1));
	}

	@Test
	public void testInheritACLs() {
		Group group = new Group();
		group.setName("parentNone");
		group.setDescription("Test group for insert method parent = none");

		Assert.assertTrue(dao.insert(group, 0));
		Assert.assertFalse(manager.getAllowedGroups(5).contains(group));

		dao.inheritACLs(group, 2);
		Assert.assertTrue(manager.getAllowedGroups(5).contains(group));
		Assert.assertFalse(manager.getAllowedGroups(9).contains(group));

		dao.inheritACLs(group, 1);
		Assert.assertTrue(manager.getAllowedGroups(9).contains(group));
	}
}