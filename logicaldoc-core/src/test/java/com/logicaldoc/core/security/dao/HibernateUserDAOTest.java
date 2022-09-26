package com.logicaldoc.core.security.dao;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.WorkingTime;
import com.logicaldoc.core.security.authentication.PasswordAlreadyUsedException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.crypt.CryptUtil;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateUserDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateUserDAOTest extends AbstractCoreTCase {

	// Instance under test
	private UserDAO dao;

	private GroupDAO groupDao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateUserDAO
		dao = (UserDAO) context.getBean("UserDAO");

		groupDao = (GroupDAO) context.getBean("GroupDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		// User with history, not deletable
		User user = dao.findByUsername("author");
		Assert.assertEquals(2, user.getGroups().size());
		dao.delete(user.getId());
		user = dao.findByUsername("author");
		Assert.assertNull(user);

		// Try with a deletable user
		User testUser = dao.findByUsername("test");
		Assert.assertEquals(2, testUser.getGroups().size());
		testUser.removeGroupMemberships(null);
		dao.store(testUser);
		dao.initialize(testUser);
		Assert.assertEquals(1, testUser.getGroups().size());

		String name = testUser.getUserGroupName();
		Assert.assertTrue(dao.delete(testUser.getId()));
		user = dao.findByUsername("test");
		Assert.assertNull(user);
		Assert.assertNull(groupDao.findByName(name, 1));

		Group group = groupDao.findByName("guest", 1);
		groupDao.initialize(group);
		Assert.assertFalse(group.getUsers().contains(testUser));
	}

	@Test
	public void testFindAll() {
		Collection<User> users = dao.findAll();
		Assert.assertNotNull(users);
		Assert.assertEquals(6, users.size());
	}

	@Test
	public void testFindByName() {
		Collection<User> users = dao.findByName("Seba%");
		Assert.assertNotNull(users);
		Assert.assertEquals(1, users.size());

		users = dao.findByName("%i%");
		Assert.assertNotNull(users);
		Assert.assertEquals(3, users.size());

		users = dao.findByName("%xxx%");
		Assert.assertNotNull(users);
		Assert.assertEquals(0, users.size());
	}

	@Test
	public void testFindByGroup() throws PersistenceException {
		Set<User> users = dao.findByGroup(1L);
		Assert.assertNotNull(users);
		Assert.assertEquals(2, users.size());
		Assert.assertTrue(users.contains(dao.findById(1L)));

		users = dao.findByGroup(99L);
		Assert.assertNotNull(users);
		Assert.assertEquals(0, users.size());
	}

	@Test
	public void testFindByUserName() {
		User user = dao.findByUsername("admin");
		Assert.assertNotNull(user);
		Assert.assertEquals("admin", user.getUsername());
		user.setDecodedPassword("admin");
		Assert.assertEquals(CryptUtil.cryptString("admin"), user.getPassword());
		Assert.assertEquals("admin@admin.net", user.getEmail());
		Assert.assertEquals(2, user.getGroups().size());

		// Try with unexisting username
		user = dao.findByUsername("xxxx");
		Assert.assertNull(user);

		user = dao.findByUsername("Admin");
		Assert.assertNull(user);
	}

	@Test
	public void testFindByUserNameIgnoreCase() {
		User user = dao.findByUsernameIgnoreCase("admin");
		Assert.assertNotNull(user);
		Assert.assertEquals("admin", user.getUsername());

		// Try with unexisting username
		user = dao.findByUsernameIgnoreCase("xxxx");
		Assert.assertNull(user);

		// Try with different case
		user = dao.findByUsername("AdMiN");
		Assert.assertNull(user);
		user = dao.findByUsernameIgnoreCase("AdMiN");
		Assert.assertNotNull(user);
		Assert.assertEquals("admin", user.getUsername());
	}

	@Test
	public void testFindByLikeUserName() {
		Collection<User> users = dao.findByLikeUsername("admin");
		Assert.assertNotNull(users);
		Assert.assertEquals(1, users.size());
		Assert.assertEquals("admin", users.iterator().next().getUsername());

		users = dao.findByLikeUsername("adm%");
		Assert.assertNotNull(users);
		Assert.assertEquals(1, users.size());
		Assert.assertEquals("admin", users.iterator().next().getUsername());

		users = dao.findByLikeUsername("xxx%");
		Assert.assertNotNull(users);
		Assert.assertTrue(users.isEmpty());
	}

	@Test
	public void testFindById() throws PersistenceException {
		User user = dao.findById(1, true);
		Assert.assertNotNull(user);
		Assert.assertEquals("admin", user.getUsername());
		user.setDecodedPassword("admin");
		Assert.assertEquals(CryptUtil.cryptString("admin"), user.getPassword());
		Assert.assertEquals("admin@admin.net", user.getEmail());
		Assert.assertEquals(2, user.getGroups().size());

		// Try with unexisting id
		user = dao.findById(9999);
		Assert.assertNull(user);
	}

	@Test
	public void testFindByUserNameAndName() {
		Collection<User> users = dao.findByUsernameAndName("boss", "Meschieri");
		Assert.assertNotNull(users);
		Assert.assertEquals(1, users.size());
		Assert.assertEquals("boss", users.iterator().next().getUsername());

		users = dao.findByUsernameAndName("b%", "Mes%");
		Assert.assertNotNull(users);
		Assert.assertEquals(1, users.size());
		Assert.assertEquals("boss", users.iterator().next().getUsername());

		users = dao.findByUsernameAndName("a%", "xxxx%");
		Assert.assertNotNull(users);
		Assert.assertTrue(users.isEmpty());
	}

	@Test
	public void testStore() throws PersistenceException {
		User user = new User();
		user.setUsername("xxx");
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		user.setName("claus");
		user.setFirstName("valca");
		user.setEmail("valca@acme.com");

		WorkingTime wt = new WorkingTime(1, 5, 30);
		user.getWorkingTimes().add(wt);

		UserHistory transaction = new UserHistory();
		transaction.setEvent(UserEvent.LOGIN.toString());
		transaction.setUserId(user.getId());
		transaction.setNotified(0);
		Assert.assertTrue(dao.store(user, transaction));
		Assert.assertNotNull(groupDao.findByName(user.getUserGroupName(), 1));

		user.addGroup(groupDao.findById(1L));
		dao.store(user);

		User storedUser = dao.findByUsername("xxx");
		Assert.assertNotNull(user);
		Assert.assertEquals(user, storedUser);
		Assert.assertEquals(2, storedUser.getGroups().size());
		Assert.assertNotNull(storedUser.getUserGroup());
		Assert.assertEquals(CryptUtil.cryptString("3$(a8BcX$7GAA%K)"), storedUser.getPassword());
		Assert.assertEquals(1, storedUser.getWorkingTimes().size());
		Assert.assertEquals(1, storedUser.getWorkingTimes().iterator().next().getDayOfWeek());
		Assert.assertEquals(5, storedUser.getWorkingTimes().iterator().next().getHourStart());
		Assert.assertEquals(30, storedUser.getWorkingTimes().iterator().next().getMinuteStart());

		user = dao.findById(1);
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		transaction = new UserHistory();
		transaction.setEvent(UserEvent.PASSWORDCHANGED.toString());
		transaction.setUserId(user.getId());
		transaction.setNotified(0);
		dao.store(user, transaction);

		user.addGroup(groupDao.findById(1L));
		user.addGroup(groupDao.findById(10L));
		dao.store(user);
		user = dao.findById(1);
		dao.initialize(user);
		Assert.assertEquals(3, user.getGroups().size());
	}

	@Test
	public void testStorePasswordChanged() throws PersistenceException {
		
		User user = dao.findById(1L);
		dao.initialize(user);
		Assert.assertEquals(0, user.getPasswordExpired());
		Assert.assertEquals(0, user.getPasswordExpires());
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		dao.store(user);

		user = dao.findById(1L);
		dao.initialize(user);
		Assert.assertEquals(0, user.getPasswordExpired());
		Assert.assertEquals(0, user.getPasswordExpires());
		user.setDecodedPassword("3$(a8BcX$7GAA%K-pippo");
		dao.store(user);

		// Give an already used password
		user = dao.findById(1L);
		dao.initialize(user);
		Assert.assertEquals(0, user.getPasswordExpired());
		Assert.assertEquals(0, user.getPasswordExpires());
		user.setDecodedPassword("3$(a8BcX$7GAA%K)");
		
		try {
			dao.store(user);
			Assert.fail("an exception should have been raised at this point");
		} catch (PasswordAlreadyUsedException e) {
		}

		user = dao.findById(1L);
		dao.initialize(user);
		Assert.assertEquals(0, user.getPasswordExpired());
		user.setDecodedPassword("3$(a8BcX$7GAA%K)-neverused");
		user.setPasswordExpired(1);
		dao.store(user);

		user = dao.findById(1L);

		PasswordHistoryDAO pDao = (PasswordHistoryDAO) Context.get().getBean(PasswordHistoryDAO.class);
		Assert.assertEquals(6, pDao.findByUserId(user.getId(), null).size());
	}

	@Test
	public void testValidateUser() {
		Assert.assertTrue(dao.validateUser("admin", "admin"));
		Assert.assertFalse(dao.validateUser("admin", "adminPWD"));
		Assert.assertFalse(dao.validateUser("xxxx", "admin"));
		Assert.assertFalse(dao.validateUser("test", "admin"));
	}

	@Test
	public void testCount() {
		Assert.assertEquals(5, dao.count(null));
	}

	public void isPasswordExpired() throws PersistenceException {
		Assert.assertFalse(dao.isPasswordExpired("admin"));
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
		Assert.assertTrue(dao.isPasswordExpired("boss"));

		calendar.add(Calendar.DAY_OF_MONTH, +2);
		lastChange = calendar.getTime();
		user.setPasswordChanged(lastChange);
		dao.store(user);
		Assert.assertFalse(dao.isPasswordExpired("boss"));
	}
}