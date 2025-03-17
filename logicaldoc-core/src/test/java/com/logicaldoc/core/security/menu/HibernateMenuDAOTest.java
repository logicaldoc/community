package com.logicaldoc.core.security.menu;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for {@link HibernateMenuDAOTest}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateMenuDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private MenuDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateMenuDAO
		testSubject = Context.get(MenuDAO.class);
	}

	@Test
	public void testStore() throws PersistenceException {
		Menu menu = new Menu();
		menu.setName("text");
		menu.setParentId(2);
		menu.setAccessControlList(Set.of(new AccessControlEntry(1L), new AccessControlEntry(2L)));
		assertNotNull(menu.getAccessControlEntry(1L));
		assertNull(menu.getAccessControlEntry(3L));
		testSubject.store(menu);
		Assert.assertNotNull(menu);

		menu = testSubject.findById(2000);
		Assert.assertEquals("menu.adminxxx", menu.getName());

		Assert.assertEquals(1, menu.getAccessControlList().size());

		// Load an existing menu and modify it
		menu = testSubject.findById(9);
		Assert.assertEquals("security", menu.getName());

		testSubject.store(menu);

		menu = testSubject.findById(101);
		menu.setName("pippo");
		testSubject.store(menu);
		Assert.assertNotNull(menu);
		menu = testSubject.findById(102);
		Assert.assertNotNull(menu);

		menu = testSubject.findById(101);
		menu.setName("pippo2");
		testSubject.store(menu);
		Assert.assertNotNull(menu);

		menu = testSubject.findById(102);
		testSubject.store(menu);
		Assert.assertNotNull(menu);
	}

	@Test
	public void testDelete() throws PersistenceException {
		testSubject.delete(99);
		Menu menu = testSubject.findById(99);
		Assert.assertNull(menu);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		docDao.delete(1);

		// Delete a folder with documents
		testSubject.delete(103);
		menu = testSubject.findById(103);
		Assert.assertNull(menu);
	}

	@Test
	public void testFindById() throws PersistenceException {
		// Try with a menu id
		Menu menu = testSubject.findById(2);
		Assert.assertNotNull(menu);
		Assert.assertEquals(2, menu.getId());
		Assert.assertEquals("administration", menu.getName());
		Assert.assertEquals("menu.png", menu.getIcon());
		Assert.assertEquals(1, menu.getAccessControlList().size());

		// Try with non-existent id
		menu = testSubject.findById(99999);
		Assert.assertNull(menu);
	}

	@Test
	public void testFindByName() {
		List<Menu> menus = (List<Menu>) testSubject.findByName(null, "abc", true);
		Assert.assertNotNull(menus);
		Assert.assertEquals(0, menus.size());

		menus = (List<Menu>) testSubject.findByName(null, "abc", false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(1, menus.size());

		// Try with non-existent text
		menus = testSubject.findByName("xxxxx");
		Assert.assertNotNull(menus);
		Assert.assertTrue(menus.isEmpty());
	}

	@Test
	public void testFindByUserNameString() throws PersistenceException {
		List<Menu> menus = testSubject.findByUserId(1);
		Assert.assertNotNull(menus);
		Assert.assertEquals(testSubject.findAllIds().size(), menus.size());

		menus = testSubject.findByUserId(3);
		Assert.assertNotNull(menus);
		Assert.assertEquals(testSubject.findAllIds().size(), menus.size());

		// Try with non-existent user
		menus = testSubject.findByUserId(99);
		Assert.assertNotNull(menus);
		Assert.assertEquals(0, menus.size());
	}

	@Test
	public void testFindByUserId() {
		List<Menu> menus = testSubject.findByUserId(1, 2, false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(9, menus.size());

		// Try with non-existent user and menus
		menus = testSubject.findByUserId(1, 999, false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(0, menus.size());

		menus = testSubject.findByUserId(99, 2, false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(0, menus.size());

		menus = testSubject.findByUserId(4, 2, false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(3, menus.size());

		menus = testSubject.findByUserId(4);
		Assert.assertNotNull(menus);
		Assert.assertEquals(28, menus.size());
	}

	@Test
	public void testFindByParentId() {
		List<Menu> menus = testSubject.findByParentId(2, false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(36, menus.size());

		// Try with non-existent parent
		menus = testSubject.findByParentId(999, false);
		Assert.assertNotNull(menus);
		Assert.assertEquals(0, menus.size());
	}

	@Test
	public void testIsWriteEnable() {
		Assert.assertTrue(testSubject.isWriteEnable(2, 1));
		Assert.assertTrue(testSubject.isWriteEnable(26, 1));
		Assert.assertTrue(testSubject.isWriteEnable(1200, 4));
		Assert.assertTrue(testSubject.isWriteEnable(2, 3));
		Assert.assertFalse(testSubject.isWriteEnable(2, 999));
	}

	@Test
	public void testIsReadEnable() {
		Assert.assertTrue(testSubject.isReadEnable(2, 1));
		Assert.assertTrue(testSubject.isReadEnable(26, 1));
		Assert.assertFalse(testSubject.isReadEnable(2, 22));
		Assert.assertFalse(testSubject.isReadEnable(2, 999));
		Assert.assertTrue(testSubject.isReadEnable(1200, 4));
	}

	@Test
	public void testFindMenuIdByUserId() {
		Collection<Long> ids = testSubject.findMenuIdByUserId(4, true);
		Assert.assertNotNull(ids);
		Assert.assertEquals(27, ids.size());
		Assert.assertTrue(ids.contains(-104L));
		Assert.assertTrue(ids.contains(1200L));

		// Try with non-existent user
		ids = testSubject.findMenuIdByUserId(99, true);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testFindIdByUserId() {
		Collection<Long> ids = testSubject.findIdByUserId(1, -101);
		Assert.assertNotNull(ids);
		Assert.assertEquals(3, ids.size());
		Assert.assertTrue(ids.contains(-103L));
		Assert.assertTrue(ids.contains(-102L));
		Assert.assertTrue(ids.contains(-104L));

		ids = testSubject.findIdByUserId(4, -101);
		Assert.assertNotNull(ids);
		Assert.assertEquals(2, ids.size());
		Assert.assertTrue(ids.contains(-103L));
		Assert.assertTrue(ids.contains(-104L));

		ids = testSubject.findIdByUserId(1, -101);
		Assert.assertNotNull(ids);
		Assert.assertEquals(3, ids.size());

		// Try with non-existent user
		ids = testSubject.findIdByUserId(99, 101);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testHasWriteAccess() throws PersistenceException {
		Menu menu = testSubject.findById(103);
		Assert.assertTrue(testSubject.hasWriteAccess(menu, 1));
		Assert.assertTrue(testSubject.hasWriteAccess(menu, 3));
		Assert.assertFalse(testSubject.hasWriteAccess(menu, 5));
		menu = testSubject.findById(-103);
		Assert.assertTrue(testSubject.hasWriteAccess(menu, 4));
		menu = testSubject.findById(-104);
		Assert.assertFalse(testSubject.hasWriteAccess(menu, 4));
	}

	@Test
	public void testFindByGroupId() throws PersistenceException {
		Collection<Menu> menus = testSubject.findByGroupId(1);
		Assert.assertEquals(testSubject.findAllIds().size(), menus.size());
		menus = testSubject.findByGroupId(10);
		Assert.assertEquals(0, menus.size());
		menus = testSubject.findByGroupId(2);
		Assert.assertTrue(menus.contains(testSubject.findById(-104)));
		Assert.assertTrue(menus.contains(testSubject.findById(1200)));
	}

	@Test
	public void testFindParents() throws PersistenceException {
		List<Menu> menus = testSubject.findParents(-103);
		Assert.assertEquals(4, menus.size());
		Assert.assertEquals(testSubject.findById(1), menus.get(0));
		Assert.assertEquals(testSubject.findById(2), menus.get(1));
		Assert.assertEquals(testSubject.findById(2000), menus.get(2));
	}

	@Test
	public void testRestore() throws PersistenceException {
		assertEquals(1000L, testSubject.queryForLong("select ld_id from ld_menu where ld_id="+1000L+" and ld_deleted=1"));
		assertEquals(1100L, testSubject.queryForLong("select ld_id from ld_menu where ld_id="+1100L+" and ld_deleted=1"));

		testSubject.restore(1100, true);
		Menu menu = testSubject.findById(1000);
		Assert.assertNotNull(menu);
		menu = testSubject.findById(1100);
		Assert.assertNotNull(menu);
	}

	@Test
	public void testFindByNameAndParentId() throws PersistenceException {
		List<Menu> menus = testSubject.findByNameAndParentId("%admin%", 2);
		Assert.assertEquals(2, menus.size());
		Assert.assertTrue(menus.contains(testSubject.findById(99)));
		Assert.assertTrue(menus.contains(testSubject.findById(2000)));
		menus = testSubject.findByNameAndParentId("text", 2000);
		Assert.assertEquals(testSubject.findById(-101), menus.get(0));
	}

	@Test
	public void testFindMenuIdByUserIdAndPermission() throws PersistenceException {
		List<Long> ids = testSubject.findMenuIdByUserIdAndPermission(4, Permission.WRITE, true);
		Assert.assertNotNull(ids);
		Assert.assertEquals(3, ids.size());
		Assert.assertTrue(ids.contains(-104L));
		Assert.assertTrue(ids.contains(1200L));
		ids = testSubject.findMenuIdByUserIdAndPermission(1, Permission.WRITE, true);
		Assert.assertNotNull(ids);
		Assert.assertEquals(testSubject.findAllIds().size(), ids.size());
		ids = testSubject.findMenuIdByUserIdAndPermission(4, Permission.WRITE, true);
		Assert.assertNotNull(ids);
		Assert.assertEquals(3, ids.size());
		Assert.assertTrue(ids.contains(-104L));
		Assert.assertTrue(ids.contains(1200L));
	}

	@Test
	public void testComputePathExtended() {
		Assert.assertEquals("/administration/test", testSubject.computePathExtended(1200));
		Assert.assertEquals("/administration/settings/outgoingemail", testSubject.computePathExtended(103));
	}

	@Test
	public void testFindChildren() throws PersistenceException {
		List<Menu> dirs = testSubject.findChildren(-101L, 1L);

		Assert.assertNotNull(dirs);
		Assert.assertEquals(3, dirs.size());

		dirs = testSubject.findChildren(-101L, 4L);
		Assert.assertNotNull(dirs);
		Assert.assertEquals(2, dirs.size());

		dirs = testSubject.findChildren(2L, 4L);
		Assert.assertNotNull(dirs);
		Assert.assertEquals(3, dirs.size());
		Assert.assertTrue(dirs.contains(testSubject.findById(1200L)));
	}

	@Test
	public void testCreatePath() throws PersistenceException {
		Menu newMenu = testSubject.createPath(72L, 1L, Menu.TYPE_CUSTOM_ACTION, "/pippo/pluto/paperino", true);
		Assert.assertNotNull(newMenu);
		Assert.assertEquals("/administration/system/general/logs/pippo/pluto/paperino",
				testSubject.computePathExtended(newMenu.getId()));
	}
}