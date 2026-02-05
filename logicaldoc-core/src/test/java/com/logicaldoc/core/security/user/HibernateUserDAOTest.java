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
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.security.PasswordGenerator;

/**
 * Test case for <code>HibernateUserDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateUserDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private UserDAO testSubject;

	private GroupDAO groupDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateUserDAO
		testSubject = UserDAO.get();
		groupDao = GroupDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		// User with history, not deletable
		User user = testSubject.findByUsername("author");
		assertEquals(3, user.getGroups().size());
		testSubject.delete(user.getId());
		user = testSubject.findByUsername("author");
		assertNull(user);

		// Try with a deletable user
		User testUser = testSubject.findByUsername("test");
		assertEquals(2, testUser.getGroups().size());
		testUser.removeGroupMemberships(null);
		testSubject.store(testUser);
		testSubject.initialize(testUser);
		assertEquals(1, testUser.getGroups().size());

		String name = testUser.getUserGroupName();
		testSubject.delete(testUser.getId());
		user = testSubject.findByUsername("test");
		assertNull(user);
		assertNull(groupDao.findByName(name, 1));

		Group group = groupDao.findByName("guest", 1);
		groupDao.initialize(group);
		assertFalse(group.getUsers().contains(testUser));
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<User> users = testSubject.findAll();
		assertNotNull(users);
		assertEquals(6, users.size());
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Collection<User> users = testSubject.findByName("Seba%");
		assertNotNull(users);
		assertEquals(1, users.size());

		users = testSubject.findByName("%i%");
		assertNotNull(users);
		assertEquals(3, users.size());

		users = testSubject.findByName("%xxx%");
		assertNotNull(users);
		assertEquals(0, users.size());
	}

	@Test
	public void testFindByGroup() throws PersistenceException {
		Set<User> users = testSubject.findByGroup(1L);
		assertNotNull(users);
		assertEquals(2, users.size());
		assertTrue(users.contains(testSubject.findById(1L)));

		users = testSubject.findByGroup(99L);
		assertNotNull(users);
		assertEquals(0, users.size());
	}

	@Test
	public void testFindByUserName() throws PersistenceException, NoSuchAlgorithmException {
		User user = testSubject.findByUsername("admin");
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
		assertEquals(CryptUtil.encryptSHA256("admin"), user.getPassword());
		assertEquals("admin@admin.net", user.getEmail());
		assertEquals(2, user.getGroups().size());

		// Try with unexisting username
		user = testSubject.findByUsername("xxxx");
		assertNull(user);

		user = testSubject.findByUsername("Admin");
		assertNull(user);
	}

	@Test
	public void testFindByUserNameIgnoreCase() throws PersistenceException {
		User user = testSubject.findByUsernameIgnoreCase("admin");
		assertNotNull(user);
		assertEquals("admin", user.getUsername());

		// Try with unexisting username
		user = testSubject.findByUsernameIgnoreCase("xxxx");
		assertNull(user);

		// Try with different case
		user = testSubject.findByUsername("AdMiN");
		assertNull(user);
		user = testSubject.findByUsernameIgnoreCase("AdMiN");
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
	}

	@Test
	public void testFindByLikeUserName() throws PersistenceException {
		Collection<User> users = testSubject.findByLikeUsername("admin");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("admin", users.iterator().next().getUsername());

		users = testSubject.findByLikeUsername("adm%");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("admin", users.iterator().next().getUsername());

		users = testSubject.findByLikeUsername("xxx%");
		assertNotNull(users);
		assertTrue(users.isEmpty());
	}

	@Test
	public void testFindById() throws PersistenceException, NoSuchAlgorithmException {
		User user = testSubject.findById(1, true);
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
		user.setDecodedPassword("admin");
		assertEquals(CryptUtil.encryptSHA256("admin"), user.getPassword());
		assertEquals("admin@admin.net", user.getEmail());
		assertEquals(2, user.getGroups().size());

		// Try with unexisting id
		user = testSubject.findById(9999);
		assertNull(user);
	}

	@Test
	public void testFindByUserNameAndName() throws PersistenceException {
		Collection<User> users = testSubject.findByUsernameAndName("boss", "Meschieri");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("boss", users.iterator().next().getUsername());

		users = testSubject.findByUsernameAndName("b%", "Mes%");
		assertNotNull(users);
		assertEquals(1, users.size());
		assertEquals("boss", users.iterator().next().getUsername());

		users = testSubject.findByUsernameAndName("a%", "xxxx%");
		assertNotNull(users);
		assertTrue(users.isEmpty());
	}

	@Test
	public void testStore() throws PersistenceException, NoSuchAlgorithmException {
		String pswd = PasswordGenerator.generate(12, 2, 2, 2, 2, 2, 2);
		User user = new User();
		user.setUsername("xxx");
		user.setDecodedPassword(pswd);
		user.setName("claus");
		user.setFirstName("<h1>valca</h1>");
		user.setEmail("valca@acme.com");

		WorkingTime wt = new WorkingTime(1, 5, 30);
		user.getWorkingTimes().add(wt);

		UserHistory transaction = new UserHistory();
		transaction.setEvent(UserEvent.LOGIN);
		transaction.setUserId(user.getId());
		transaction.setNotified(false);
		testSubject.store(user, transaction);
		assertNotNull(groupDao.findByName(user.getUserGroupName(), 1));

		assertEquals("valca", user.getFirstName());

		user = testSubject.findById(user.getId());
		testSubject.initialize(user);
		user.addGroup(groupDao.findById(1L));
		testSubject.store(user);

		User storedUser = testSubject.findByUsername("xxx");
		testSubject.initialize(storedUser);
		assertNotNull(user);
		assertEquals(user, storedUser);
		assertEquals(2, storedUser.getGroups().size());
		assertNotNull(storedUser.getUserGroup());
		assertEquals(CryptUtil.encryptSHA256(pswd), storedUser.getPassword());
		assertEquals(1, storedUser.getWorkingTimes().size());
		assertEquals(1, storedUser.getWorkingTimes().iterator().next().getDayOfWeek());
		assertEquals(5, storedUser.getWorkingTimes().iterator().next().getHourStart());
		assertEquals(30, storedUser.getWorkingTimes().iterator().next().getMinuteStart());

		user = testSubject.findById(1);
		user.setDecodedPassword(PasswordGenerator.generate(12, 2, 2, 2, 2, 2, 2));
		transaction = new UserHistory();
		transaction.setEvent(UserEvent.PASSWORDCHANGED);
		transaction.setUserId(user.getId());
		transaction.setNotified(false);
		testSubject.store(user, transaction);

		user = testSubject.findById(1);
		user.addGroup(groupDao.findById(1L));
		user.addGroup(groupDao.findById(10L));
		testSubject.store(user);
		user = testSubject.findById(1);
		testSubject.initialize(user);
		assertEquals(3, user.getGroups().size());
	}

	@Test
	public void testStorePasswordChanged() throws PersistenceException, NoSuchAlgorithmException {
		String pswd = PasswordGenerator.generate(12, 2, 2, 2, 2, 2, 2);

		User user = testSubject.findById(1L);
		testSubject.initialize(user);
		assertFalse(user.isPasswordExpired());
		assertFalse(user.isPasswordExpires());
		user.setDecodedPassword(pswd);

		testSubject.store(user);

		user = testSubject.findById(1L);
		testSubject.initialize(user);
		assertFalse(user.isPasswordExpired());
		assertFalse(user.isPasswordExpires());
		user.setDecodedPassword(pswd + "-*/!?");
		testSubject.store(user);

		// Give an already used password
		user = testSubject.findById(1L);
		testSubject.initialize(user);
		assertFalse(user.isPasswordExpired());
		assertFalse(user.isPasswordExpires());
		user.setDecodedPassword(pswd);

		try {
			testSubject.store(user);
			fail("an exception should have been raised at this point");
		} catch (PasswordAlreadyUsedException e) {
			// Nothing to do
		}

		user = testSubject.findById(1L);
		testSubject.initialize(user);
		assertFalse(user.isPasswordExpired());
		user.setDecodedPassword(pswd + "-#é+^");
		user.setPasswordExpired(true);
		testSubject.store(user);

		user = testSubject.findById(1L);

		PasswordHistoryDAO pDao = (PasswordHistoryDAO) PasswordHistoryDAO.get();
		assertEquals(6, pDao.findByUserId(user.getId(), null).size());
	}

	@Test
	public void testCountRegulars() throws PersistenceException {
		assertEquals(5, testSubject.countRegulars(null));
	}

	public void isPasswordExpired() throws PersistenceException {
		assertFalse(testSubject.isPasswordExpired("admin"));
		User user = testSubject.findByUsername("boss");
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
		testSubject.store(user);
		assertTrue(testSubject.isPasswordExpired("boss"));

		calendar.add(Calendar.DAY_OF_MONTH, +2);
		lastChange = calendar.getTime();
		user.setPasswordChanged(lastChange);
		testSubject.store(user);
		assertFalse(testSubject.isPasswordExpired("boss"));
	}
}