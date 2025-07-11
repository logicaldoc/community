package com.logicaldoc.core.security.menu;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

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
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

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
		assertNotNull(menu);

		menu = testSubject.findById(2000);
		assertEquals("menu.adminxxx", menu.getName());

		assertEquals(1, menu.getAccessControlList().size());

		// Load an existing menu and modify it
		menu = testSubject.findById(9);
		assertEquals("security", menu.getName());

		testSubject.store(menu);

		menu = testSubject.findById(101);
		menu.setName("pippo");
		testSubject.store(menu);
		assertNotNull(menu);
		menu = testSubject.findById(102);
		assertNotNull(menu);

		menu = testSubject.findById(101);
		menu.setName("pippo2");
		testSubject.store(menu);
		assertNotNull(menu);

		menu = testSubject.findById(102);
		testSubject.store(menu);
		assertNotNull(menu);
	}

	@Test
	public void testDelete() throws PersistenceException {
		testSubject.delete(99);
		Menu menu = testSubject.findById(99);
		assertNull(menu);

		DocumentDAO docDao = Context.get(DocumentDAO.class);
		docDao.delete(1);

		// Delete a folder with documents
		testSubject.delete(103);
		menu = testSubject.findById(103);
		assertNull(menu);
	}

	@Test
	public void testFindById() throws PersistenceException {
		// Try with a menu id
		Menu menu = testSubject.findById(2);
		assertNotNull(menu);
		assertEquals(2, menu.getId());
		assertEquals("administration", menu.getName());
		assertEquals("menu.png", menu.getIcon());
		assertEquals(1, menu.getAccessControlList().size());

		// Try with non-existent id
		menu = testSubject.findById(99999);
		assertNull(menu);
	}

	@Test
	public void testFindByName() {
		List<Menu> menus = (List<Menu>) testSubject.findByName(null, "abc", true);
		assertNotNull(menus);
		assertEquals(0, menus.size());

		menus = (List<Menu>) testSubject.findByName(null, "abc", false);
		assertNotNull(menus);
		assertEquals(1, menus.size());

		// Try with non-existent text
		menus = testSubject.findByName("xxxxx");
		assertNotNull(menus);
		assertTrue(menus.isEmpty());
	}

	@Test
	public void testFindByUserNameString() throws PersistenceException {
		List<Menu> menus = testSubject.findByUserId(1);
		assertNotNull(menus);
		assertEquals(testSubject.findAllIds().size(), menus.size());

		menus = testSubject.findByUserId(3);
		assertNotNull(menus);
		assertEquals(testSubject.findAllIds().size(), menus.size());

		// Try with non-existent user
		menus = testSubject.findByUserId(99);
		assertNotNull(menus);
		assertEquals(0, menus.size());
	}

	@Test
	public void testFindByUserId() {
		List<Menu> menus = testSubject.findByUserId(1, 2, false);
		assertNotNull(menus);
		assertEquals(9, menus.size());

		// Try with non-existent user and menus
		menus = testSubject.findByUserId(1, 999, false);
		assertNotNull(menus);
		assertEquals(0, menus.size());

		menus = testSubject.findByUserId(99, 2, false);
		assertNotNull(menus);
		assertEquals(0, menus.size());

		menus = testSubject.findByUserId(4, 2, false);
		assertNotNull(menus);
		assertEquals(3, menus.size());

		menus = testSubject.findByUserId(4);
		assertNotNull(menus);
		assertEquals(29, menus.size());
	}

	@Test
	public void testFindByParentId() {
		List<Menu> menus = testSubject.findByParentId(2, false);
		assertNotNull(menus);
		assertEquals(37, menus.size());

		// Try with non-existent parent
		menus = testSubject.findByParentId(999, false);
		assertNotNull(menus);
		assertEquals(0, menus.size());
	}

	@Test
	public void testIsWriteEnable() {
		assertTrue(testSubject.isWriteEnable(2, 1));
		assertTrue(testSubject.isWriteEnable(26, 1));
		assertTrue(testSubject.isWriteEnable(1200, 4));
		assertTrue(testSubject.isWriteEnable(2, 3));
		assertFalse(testSubject.isWriteEnable(2, 999));
	}

	@Test
	public void testIsReadEnable() {
		assertTrue(testSubject.isReadEnable(2, 1));
		assertTrue(testSubject.isReadEnable(26, 1));
		assertFalse(testSubject.isReadEnable(2, 22));
		assertFalse(testSubject.isReadEnable(2, 999));
		assertTrue(testSubject.isReadEnable(1200, 4));
	}

	@Test
	public void testFindMenuIdByUserId() {
		Collection<Long> ids = testSubject.findMenuIdByUserId(4, true);
		assertNotNull(ids);
		assertEquals(28, ids.size());
		assertTrue(ids.contains(-104L));
		assertTrue(ids.contains(1200L));

		// Try with non-existent user
		ids = testSubject.findMenuIdByUserId(99, true);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindIdByUserId() {
		Collection<Long> ids = testSubject.findIdByUserId(1, -101);
		assertNotNull(ids);
		assertEquals(3, ids.size());
		assertTrue(ids.contains(-103L));
		assertTrue(ids.contains(-102L));
		assertTrue(ids.contains(-104L));

		ids = testSubject.findIdByUserId(4, -101);
		assertNotNull(ids);
		assertEquals(2, ids.size());
		assertTrue(ids.contains(-103L));
		assertTrue(ids.contains(-104L));

		ids = testSubject.findIdByUserId(1, -101);
		assertNotNull(ids);
		assertEquals(3, ids.size());

		// Try with non-existent user
		ids = testSubject.findIdByUserId(99, 101);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testHasWriteAccess() throws PersistenceException {
		Menu menu = testSubject.findById(103);
		assertTrue(testSubject.hasWriteAccess(menu, 1));
		assertTrue(testSubject.hasWriteAccess(menu, 3));
		assertFalse(testSubject.hasWriteAccess(menu, 5));
		menu = testSubject.findById(-103);
		assertTrue(testSubject.hasWriteAccess(menu, 4));
		menu = testSubject.findById(-104);
		assertFalse(testSubject.hasWriteAccess(menu, 4));
	}

	@Test
	public void testFindByGroupId() throws PersistenceException {
		Collection<Menu> menus = testSubject.findByGroupId(1);
		assertEquals(testSubject.findAllIds().size(), menus.size());
		menus = testSubject.findByGroupId(10);
		assertEquals(0, menus.size());
		menus = testSubject.findByGroupId(2);
		assertTrue(menus.contains(testSubject.findById(-104)));
		assertTrue(menus.contains(testSubject.findById(1200)));
	}

	@Test
	public void testFindParents() throws PersistenceException {
		List<Menu> menus = testSubject.findParents(-103);
		assertEquals(4, menus.size());
		assertEquals(testSubject.findById(1), menus.get(0));
		assertEquals(testSubject.findById(2), menus.get(1));
		assertEquals(testSubject.findById(2000), menus.get(2));
	}

	@Test
	public void testRestore() throws PersistenceException {
		assertEquals(1000L,
				testSubject.queryForLong("select ld_id from ld_menu where ld_id=" + 1000L + " and ld_deleted=1"));
		assertEquals(1100L,
				testSubject.queryForLong("select ld_id from ld_menu where ld_id=" + 1100L + " and ld_deleted=1"));

		testSubject.restore(1100, true);
		Menu menu = testSubject.findById(1000);
		assertNotNull(menu);
		menu = testSubject.findById(1100);
		assertNotNull(menu);
	}

	@Test
	public void testFindByNameAndParentId() throws PersistenceException {
		List<Menu> menus = testSubject.findByNameAndParentId("%admin%", 2);
		assertEquals(2, menus.size());
		assertTrue(menus.contains(testSubject.findById(98)));
		assertTrue(menus.contains(testSubject.findById(2000)));
		menus = testSubject.findByNameAndParentId("text", 2000);
		assertEquals(testSubject.findById(-101), menus.get(0));
	}

	@Test
	public void testFindMenuIdByUserIdAndPermission() throws PersistenceException {
		List<Long> ids = testSubject.findMenuIdByUserIdAndPermission(4, Permission.WRITE, true);
		assertNotNull(ids);
		assertEquals(3, ids.size());
		assertTrue(ids.contains(-104L));
		assertTrue(ids.contains(1200L));
		ids = testSubject.findMenuIdByUserIdAndPermission(1, Permission.WRITE, true);
		assertNotNull(ids);
		assertEquals(testSubject.findAllIds().size(), ids.size());
		ids = testSubject.findMenuIdByUserIdAndPermission(4, Permission.WRITE, true);
		assertNotNull(ids);
		assertEquals(3, ids.size());
		assertTrue(ids.contains(-104L));
		assertTrue(ids.contains(1200L));
	}

	@Test
	public void testComputePathExtended() {
		assertEquals("/administration/test", testSubject.computePathExtended(1200));
		assertEquals("/administration/settings/outgoingemail", testSubject.computePathExtended(103));
	}

	@Test
	public void testFindChildren() throws PersistenceException {
		List<Menu> dirs = testSubject.findChildren(-101L, 1L);

		assertNotNull(dirs);
		assertEquals(3, dirs.size());

		dirs = testSubject.findChildren(-101L, 4L);
		assertNotNull(dirs);
		assertEquals(2, dirs.size());

		dirs = testSubject.findChildren(2L, 4L);
		assertNotNull(dirs);
		assertEquals(3, dirs.size());
		assertTrue(dirs.contains(testSubject.findById(1200L)));
	}

	@Test
	public void testCreatePath() throws PersistenceException {
		Menu newMenu = testSubject.createPath(72L, 1L, Menu.TYPE_CUSTOM_ACTION, "/pippo/pluto/paperino", true);
		assertNotNull(newMenu);
		assertEquals("/administration/system/general/logs/pippo/pluto/paperino",
				testSubject.computePathExtended(newMenu.getId()));
	}
}