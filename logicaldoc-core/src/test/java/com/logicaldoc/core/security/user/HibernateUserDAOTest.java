package com.logicaldoc.core.security.user;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.sql.SQLException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.PasswordAlreadyUsedException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateUserDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateUserDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private UserDAO dao;

	private GroupDAO groupDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateUserDAO
		dao = Context.get(UserDAO.class);
		groupDao = Context.get(GroupDAO.class);
	}

	@Test
	public void testDelete() throws PersistenceException {
		// User with history, not deletable
		User user = dao.findByUsername("author");
		assertEquals(3, user.getGroups().size());
		dao.delete(user.getId());
		user = dao.findByUsername("author");
		assertNull(user);

		// Try with a deletable user
		User testUser = dao.findByUsername("test");
		assertEquals(2, testUser.getGroups().size());
		testUser.removeGroupMemberships(null);
		dao.store(testUser);
		dao.initialize(testUser);
		assertEquals(1, testUser.getGroups().size());

		String name = testUser.getUserGroupName();
		dao.delete(testUser.getId());
		user = dao.findByUsername("test");
		assertNull(user);
		assertNull(groupDao.findByName(name, 1));

		Group group = groupDao.findByName("guest", 1);
		groupDao.initialize(group);
		assertFalse(group.getUsers().contains(testUser));
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<User> users = dao.findAll();
		assertNotNull(users);
		assertEquals(6, users.size());
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Collection<User> users = dao.findByName("Seba%");
		assertNotNull(users);
		assertEquals(1, users.size());

		users = dao.findByName("%i%");
		assertNotNull(users);
		assertEquals(3, users.size());

		users = dao.findByName("%xxx%");
		assertNotNull(users);
		assertEquals(0, users.size());
	}

	@Test
	public void testFindByGroup() throws PersistenceException {
		Set<User> users = dao.findByGroup(1L);
		assertNotNull(users);
		assertEquals(2, users.size());
		assertTrue(users.contains(dao.findById(1L)));

		users = dao.findByGroup(99L);
		assertNotNull(users);
		assertEquals(0, users.size());
	}

	@Test
	public void testFindByUserName() throws PersistenceException, NoSuchAlgorithmException {
		User user = dao.findByUsername("admin");
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
		user.setDecodedPassword("admin");
		assertEquals(CryptUtil.encryptSHA256("admin"), user.getPassword());
		assertEquals("admin@admin.net", user.getEmail());
		assertEquals(2, user.getGroups().size());

		// Try with unexisting username
		user = dao.findByUsername("xxxx");
		assertNull(user);

		user = dao.findByUsername("Admin");
		assertNull(user);
	}

	@Test
	public void testFindByUserNameIgnoreCase() throws PersistenceException {
		User user = dao.findByUsernameIgnoreCase("admin");
		assertNotNull(user);
		assertEquals("admin", user.getUsername());

		// Try with unexisting username
		user = dao.findByUsernameIgnoreCase("xxxx");
		assertNull(user);

		// Try with different case
		user = dao.findByUsername("AdMiN");
		assertNull(user);
		user = dao.findByUsernameIgnoreCase("AdMiN");
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
	}

	@Test
	public void testFindByLikeUserName() throws PersistenceException {
		Collection<User> users = dao.findByLikeUsername("admin");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("admin", users.iterator().next().getUsername());

		users = dao.findByLikeUsername("adm%");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("admin", users.iterator().next().getUsername());

		users = dao.findByLikeUsername("xxx%");
		assertNotNull(users);
		assertTrue(users.isEmpty());
	}

	@Test
	public void testFindById() throws PersistenceException, NoSuchAlgorithmException {
		User user = dao.findById(1, true);
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
		user.setDecodedPassword("admin");
		assertEquals(CryptUtil.encryptSHA256("admin"), user.getPassword());
		assertEquals("admin@admin.net", user.getEmail());
		assertEquals(2, user.getGroups().size());

		// Try with unexisting id
		user = dao.findById(9999);
		assertNull(user);
	}

	@Test
	public void testFindByUserNameAndName() throws PersistenceException {
		Collection<User> users = dao.findByUsernameAndName("boss", "Meschieri");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("boss", users.iterator().next().getUsername());

		users = dao.findByUsernameAndName("b%", "Mes%");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("boss", users.iterator().next().getUsername());

		users = dao.findByUsernameAndName("a%", "xxxx%");
		assertNotNull(users);
		assertTrue(users.isEmpty());
	}

	@Test
	public void testStore() throws PersistenceException, NoSuchAlgorithmException {
		User user = new User();
		user.setUsername("xxx");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		user.setName("claus");
		user.setFirstName("valca");
		user.setEmail("valca@acme.com");

		WorkingTime wt = new WorkingTime(1, 5, 30);
		user.getWorkingTimes().add(wt);

		UserHistory transaction = new UserHistory();
		transaction.setEvent(UserEvent.LOGIN);
		transaction.setUserId(user.getId());
		transaction.setNotified(0);
		dao.store(user, transaction);
		assertNotNull(groupDao.findByName(user.getUserGroupName(), 1));

		user.addGroup(groupDao.findById(1L));
		dao.store(user);

		User storedUser = dao.findByUsername("xxx");
		dao.initialize(storedUser);
		assertNotNull(user);
		assertEquals(user, storedUser);
		assertEquals(2, storedUser.getGroups().size());
		assertNotNull(storedUser.getUserGroup());
		assertEquals(CryptUtil.encryptSHA256("3$(a8BcX$7GAA%K)"), storedUser.getPassword());
		assertEquals(1, storedUser.getWorkingTimes().size());
		assertEquals(1, storedUser.getWorkingTimes().iterator().next().getDayOfWeek());
		assertEquals(5, storedUser.getWorkingTimes().iterator().next().getHourStart());
		assertEquals(30, storedUser.getWorkingTimes().iterator().next().getMinuteStart());

		user = dao.findById(1);
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		transaction = new UserHistory();
		transaction.setEvent(UserEvent.PASSWORDCHANGED);
		transaction.setUserId(user.getId());
		transaction.setNotified(0);
		dao.store(user, transaction);

		user.addGroup(groupDao.findById(1L));
		user.addGroup(groupDao.findById(10L));
		dao.store(user);
		user = dao.findById(1);
		dao.initialize(user);
		assertEquals(3, user.getGroups().size());
	}

	@Test
	public void testStorePasswordChanged() throws PersistenceException, NoSuchAlgorithmException {

		User user = dao.findById(1L);
		dao.initialize(user);
		assertEquals(0, user.getPasswordExpired());
		assertEquals(0, user.getPasswordExpires());
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		dao.store(user);

		user = dao.findById(1L);
		dao.initialize(user);
		assertEquals(0, user.getPasswordExpired());
		assertEquals(0, user.getPasswordExpires());
		user.setDecodedPassword("3$(a8BcX$7GAA%K-pippo");
		dao.store(user);

		// Give an already used password
		user = dao.findById(1L);
		dao.initialize(user);
		assertEquals(0, user.getPasswordExpired());
		assertEquals(0, user.getPasswordExpires());
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");

		try {
			dao.store(user);
			fail("an exception should have been raised at this point");
		} catch (PasswordAlreadyUsedException e) {
			// Nothing to do
		}

		user = dao.findById(1L);
		dao.initialize(user);
		assertEquals(0, user.getPasswordExpired());
		user.setDecodedPassword("3$(a8BcX$7GAA%K)-neverused");
		user.setPasswordExpired(1);
		dao.store(user);

		user = dao.findById(1L);

		PasswordHistoryDAO pDao = (PasswordHistoryDAO) Context.get(PasswordHistoryDAO.class);
		assertEquals(6, pDao.findByUserId(user.getId(), null).size());
	}

	@Test
	public void testCount() throws PersistenceException {
		assertEquals(5, dao.count(null));
	}

	public void isPasswordExpired() throws PersistenceException {
		assertFalse(dao.isPasswordExpired("admin"));
		User user = dao.findByUsername("boss");
		Date lastChange = null;
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(lastChange);
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR, 0);
		calendar.add(Calendar.DAY_OF_MONTH, -91);
		lastChange = calendar.getTime();
		user.setPasswordChanged(lastChange);
		dao.store(user);
		assertTrue(dao.isPasswordExpired("boss"));

		calendar.add(Calendar.DAY_OF_MONTH, +2);
		lastChange = calendar.getTime();
		user.setPasswordChanged(lastChange);
		dao.store(user);
		assertFalse(dao.isPasswordExpired("boss"));
	}
}