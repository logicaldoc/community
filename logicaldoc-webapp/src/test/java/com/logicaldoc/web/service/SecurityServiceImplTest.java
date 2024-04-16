package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.time.DateUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.BeanUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.DeviceDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIWorkingTime;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWebappTestCase;
import com.logicaldoc.web.UploadServlet;

public class SecurityServiceImplTest extends AbstractWebappTestCase {

	// Instance under test
	private SecurityServiceImpl testSubject = new SecurityServiceImpl();

	private UserDAO userDAO;

	private GroupDAO groupDAO;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();

		userDAO = (UserDAO) context.getBean("UserDAO");
		groupDAO = (GroupDAO) context.getBean("GroupDAO");
	}

	@Test
	public void testLogout() {
		assertEquals("admin", guiSession.getUser().getUsername());
		assertEquals(1, guiSession.getUser().getId());
		int sessions = SessionManager.get().countOpened();
		testSubject.logout();

		assertEquals(sessions - 1, SessionManager.get().countOpened());
	}

	@Test
	public void testChangePassword() {
		assertEquals("0", testSubject.changePassword(1L, 1L, "admin", "TBDcy@u<QOR;6}l", false).getCode());
		assertEquals("0", testSubject.changePassword(1L, 1L, "TBDcy@u<QOR;6}l", "TBDcy@u<QOR;6}l;", false).getCode());
		assertEquals("0", testSubject.changePassword(1L, 1L, "TBDcy@u<QOR;6}l;", "TBDcy@u<QOR;6}l-", false).getCode());
	}

	@Test
	public void testGetSession() {
		GUISession guiSession = testSubject.getSession("en", session.getSid());
		assertEquals(session.getSid(), guiSession.getSid());
	}

	@Test
	public void testAddUserToGroup() throws ServerException, PersistenceException {
		User test = userDAO.findByUsername("test");
		assertNotNull(test);
		Group group = groupDAO.findByName("author", Tenant.DEFAULT_ID);
		assertNotNull(group);
		testSubject.addUserToGroup(group.getId(), test.getId());
		User user = userDAO.findByUsername("test");
		assertTrue(user.getGroups().contains(group));

		group = groupDAO.findByName("guest", Tenant.DEFAULT_ID);
		assertNotNull(group);
		testSubject.addUserToGroup(group.getId(), test.getId());
		user = userDAO.findByUsername("test");
		assertTrue(user.getGroups().contains(group));
	}

	@Test
	public void testDeleteGroup() throws ServerException, PersistenceException {
		assertNotNull(groupDAO.findById(10));
		testSubject.deleteGroup(10);
		assertNull(groupDAO.findById(10));

		// Delete a non-deleteable group
		assertNotNull(groupDAO.findById(1));
		try {
			testSubject.deleteGroup(1);
			fail("Admin group has been deleted");
		} catch (Exception e) {
			// We expect an exception here
		}
		assertNotNull(groupDAO.findById(1));
	}

	@Test
	public void testDeleteUser() throws ServerException, PersistenceException {
		User user = userDAO.findByUsername("author");
		assertEquals(2, user.getGroups().size());
		testSubject.deleteUser(user.getId());
		user = userDAO.findByUsername("author");
		assertNull(user);
	}

	@Test
	public void testRemoveFromGroup() throws ServerException, PersistenceException {
		List<Long> users = List.of(5L, 1L);
		Group group = groupDAO.findByName("author", Tenant.DEFAULT_ID);
		testSubject.removeFromGroup(group.getId(), users);
		User user = userDAO.findByUsername("test");
		assertFalse(user.getGroups().contains(group));
		user = userDAO.findByUsername("admin");
		assertFalse(user.getGroups().contains(group));
	}

	@Test
	public void testGetGroup() throws ServerException {
		GUIGroup group = testSubject.getGroup(10);
		assertNotNull(group);
		assertEquals("testGroup", group.getName());

		// Try with unexisting id
		group = testSubject.getGroup(999);
		assertNull(group);
	}

	@Test
	public void testGetUser() throws ServerException {
		GUIUser user = testSubject.getUser(1);
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
		assertEquals("admin@admin.net", user.getEmail());

		user = testSubject.getUser(3);
		assertNotNull(user);
		assertEquals("sebastian", user.getUsername());
		assertEquals("seb_stein@gmx.de", user.getEmail());
		assertEquals("de", user.getLanguage());

		// Try with unexisting id
		user = testSubject.getUser(9999);
		assertNull(user);
	}

	@Test
	public void testSaveGroup() throws ServerException {
		GUIGroup group = testSubject.getGroup(10);
		group = testSubject.saveGroup(group);
		assertNotNull(group);
		assertEquals("testGroup", group.getName());

		group = new GUIGroup();
		group.setName("Dummy");
		group.setInheritGroupId(2L);
		group = testSubject.saveGroup(group);
		group = testSubject.getGroup(group.getId());
		assertNotNull(group);
		assertEquals("Dummy", group.getName());
	}

	@Test
	public void testSaveUser() throws ServerException {
		GUIUser user = testSubject.getUser(1);

		user = testSubject.saveUser(user, guiSession.getInfo());
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
		assertEquals("admin@admin.net", user.getEmail());

		user = testSubject.getUser(3);

		user = testSubject.saveUser(user, guiSession.getInfo());
		assertNotNull(user);
		assertEquals("sebastian", user.getUsername());
		assertEquals("seb_stein@gmx.de", user.getEmail());
		assertEquals("de", user.getLanguage());

		GUIUser newUser = new GUIUser();
		BeanUtils.copyProperties(user, newUser);
		newUser.setId(0L);
		newUser.setUsername("dummy");
		newUser.setExpire(DateUtils.addDays(new Date(), 30));

		GUIWorkingTime wt = new GUIWorkingTime("workl", new Date(), DateUtils.addHours(new Date(), 1));
		newUser.getWorkingTimes().add(wt);
		newUser = testSubject.saveUser(newUser, guiSession.getInfo());
		newUser = testSubject.getUser(newUser.getId());
		assertNotNull(newUser);
		assertEquals("dummy", newUser.getUsername());

		// Save a new user with same username
		newUser.setId(0L);
		user = testSubject.saveUser(newUser, guiSession.getInfo());
		assertEquals(0L, user.getId());
	}

	@Test
	public void testSaveProfile() throws ServerException {
		GUIUser user = testSubject.getUser(3);
		user.setCity("Carpi");
		user = testSubject.saveProfile(user);

		user = testSubject.getUser(3);
		assertEquals("Carpi", user.getCity());
	}

	@Test
	public void testSaveInterfaceSettings() throws ServerException {
		GUIUser user = testSubject.getUser(3);
		user.setDefaultWorkspace(99L);
		user = testSubject.saveInterfaceSettings(user);

		user = testSubject.getUser(3);
		assertEquals(99L, user.getDefaultWorkspace().longValue());
	}

	@Test
	public void testLoadSettings() throws ServerException {
		GUISecuritySettings settings = testSubject.loadSettings();
		assertEquals(2, settings.getPwdUpperCase());
	}

	@Test
	public void testSaveACL() throws ServerException {
		GUIMenu menu = testSubject.getMenu(-102, "en");
		assertTrue(menu.getAccessControlList().isEmpty());
		menu.getAccessControlList().add(new GUIAccessControlEntry(3L, GUIAccessControlEntry.PERMISSION_READ));
		testSubject.saveACL(menu);

		menu = testSubject.getMenu(-102L, "en");
		assertEquals(1, menu.getAccessControlList().size());
		assertTrue(menu.getAccessControlList().stream().anyMatch(ace -> ace.getEntityId() == 3L));
	}

	@Test
	public void testSaveMenus() throws ServerException {
		List<GUIMenu> menus = testSubject.getMenus(-101L, "en", false);
		assertEquals(2, menus.size());

		GUIMenu newMenu = new GUIMenu();
		newMenu.setName("Dummy");
		newMenu.setParentId(-101L);
		newMenu.getAccessControlList().add(new GUIAccessControlEntry(3L, GUIAccessControlEntry.PERMISSION_READ));
		testSubject.saveMenus(List.of(newMenu), "en");

		menus = testSubject.getMenus(-101L, "en", false);
		assertEquals(3, menus.size());
		assertTrue(menus.stream().anyMatch(m -> "Dummy".equals(m.getName())));
	}

	@Test
	public void testDeleteMenu() throws ServerException {
		List<GUIMenu> menus = testSubject.getMenus(-101L, "en", false);
		assertEquals(2, menus.size());

		testSubject.deleteMenu(-103l);

		menus = testSubject.getMenus(-101L, "en", false);
		assertEquals(1, menus.size());
	}

	@Test
	public void testKill() {
		SessionManager sm = SessionManager.get();
		sm.clear();
		Session session1 = sm.newSession("admin", "admin", null);
		assertNotNull(session1);
		Session session2 = sm.newSession("admin", "admin", null);
		assertNotNull(session2);
		assertNotSame(session1, session2);
		assertEquals(2, sm.getSessions().size());

		testSubject.kill(session1.getSid());
		assertTrue(sm.isOpen(session2.getSid()));
		assertFalse(sm.isOpen(session1.getSid()));
		assertEquals(2, sm.getSessions().size());
	}

	@Test
	public void testSaveSettings() {
		GUISecuritySettings securitySettings = new GUISecuritySettings();
		securitySettings.setPwdExpiration(30);
		securitySettings.setPwdSize(6);
		securitySettings.setAnonymousKey("xxx");

		String notThrownTest = null;
		try {
			testSubject.saveSettings(securitySettings);
			notThrownTest = "ok";
		} catch (Exception t) {
			t.printStackTrace();
			// Nothing to do
		}
		assertEquals("ok", notThrownTest);
	}

	@Test
	public void testSearchUsers() throws ServerException {
		List<GUIUser> users = testSubject.searchUsers(null, null);
		assertEquals(5, users.size());

		users = testSubject.searchUsers("admin", null);
		assertEquals(1, users.size());

		users = testSubject.searchUsers("admin", "1");
		assertEquals(1, users.size());

		users = testSubject.searchUsers("admin", "999");
		assertTrue(users.isEmpty());

		users = testSubject.searchUsers("unexisting", "1");
		assertTrue(users.isEmpty());

		users = testSubject.searchUsers("unexisting", null);
		assertTrue(users.isEmpty());

		users = testSubject.searchUsers(null, "1");
		assertEquals(2, users.size());
	}

	@Test
	public void testLoadBlockedEntities() throws ServerException {
		List<GUISequence> sequences = testSubject.loadBlockedEntities();
		assertEquals(4, sequences.size());
	}

	@Test
	public void testRemoveBlockedEntities() throws ServerException {
		List<GUISequence> sequences = testSubject.loadBlockedEntities();
		assertEquals(4, sequences.size());
		testSubject.removeBlockedEntities(List.of(sequences.get(0).getId()));
		sequences = testSubject.loadBlockedEntities();
		assertEquals(3, sequences.size());
	}

	@Test
	public void testReplicateUsersSettings() throws ServerException {
		GUIUser user = testSubject.getUser(2L);
		assertFalse(user.isMemberOf("admin"));

		testSubject.replicateUsersSettings(1L, List.of(2L), true, true);

		user = testSubject.getUser(2L);
		assertTrue(user.isMemberOf("admin"));
	}

	@Test
	public void testTrustDevice() throws ServerException {
		String deviceId = testSubject.trustDevice("myLabel");

		// Without second factor it is always trusted
		assertTrue(testSubject.isTrustedDevice(deviceId));

		// Now enable the second factor to check the device
		session.getUser().setSecondFactor("xyz");
		deviceId = testSubject.trustDevice("myLabel");
		assertTrue(testSubject.isTrustedDevice(deviceId));
	}

	@Test
	public void testDeleteTrustedDevices() throws ServerException {
		// Now enable the second factor to check the device
		session.getUser().setSecondFactor("xyz");
		String deviceId = testSubject.trustDevice("myLabel");
		assertTrue(testSubject.isTrustedDevice(deviceId));

		DeviceDAO dDao = (DeviceDAO) Context.get().getBean(DeviceDAO.class);
		Device device = dDao.findByDeviceId(deviceId);
		testSubject.deleteTrustedDevices(List.of(device.getId()));
		assertFalse(testSubject.isTrustedDevice(deviceId));
	}

	@Test
	public void testDeviceLabel() throws ServerException {
		String deviceId = testSubject.trustDevice("myLabel");
		assertTrue(testSubject.isTrustedDevice(deviceId));
		DeviceDAO dDao = (DeviceDAO) Context.get().getBean(DeviceDAO.class);
		Device device = dDao.findByDeviceId(deviceId);
		assertNotNull(device);
		assertEquals("myLabel", device.getLabel());

		testSubject.updateDeviceLabel(device.getId(), "myDevice");
		device = dDao.findByDeviceId(deviceId);
		assertNotNull(device);
		assertEquals("myDevice", device.getLabel());
	}

	@Test
	public void testSaveAvatar() throws ServerException, PersistenceException, IOException {
		User user = userDAO.findById(1L);
		String oldAvatar = user.getAvatar();

		File avatarFile = new File("target/avatar.png");
		FileUtil.copyResource("/avatar.png", avatarFile);
		try {
			Map<String, File> uploadedFilesMap = new HashMap<>();
			uploadedFilesMap.put(avatarFile.getName(), avatarFile);
			session.getDictionary().put(UploadServlet.RECEIVED_FILES, uploadedFilesMap);

			testSubject.saveAvatar(1L);
			user = userDAO.findById(1L);
			assertNotSame(oldAvatar, user.getAvatar());
		} finally {
			FileUtil.strongDelete(avatarFile);
		}
	}

	@Test
	public void testResetAvatar() throws ServerException, PersistenceException {
		User user = userDAO.findById(1L);
		userDAO.initialize(user);
		user.setAvatar("xyz");
		userDAO.store(user);

		testSubject.resetAvatar(1L);
		user = userDAO.findById(1L);
		assertNotSame("zyz", user.getAvatar());
	}

	@Test
	public void testCloneWorkTimes() throws ServerException, PersistenceException {
		GUIUser user = testSubject.getUser(3L);
		assertTrue(user.getWorkingTimes().isEmpty());

		GUIUser newUser = new GUIUser();
		BeanUtils.copyProperties(user, newUser);
		newUser.setId(0L);
		newUser.setUsername("dummy");
		newUser.setExpire(DateUtils.addDays(new Date(), 30));

		GUIWorkingTime wt = new GUIWorkingTime("workl", new Date(), DateUtils.addHours(new Date(), 1));
		newUser.getWorkingTimes().add(wt);
		assertTrue(newUser.getWorkingTimes().stream().anyMatch(ww -> "workl".equals(ww.getLabel())));

		newUser = testSubject.saveUser(newUser, guiSession.getInfo());
		newUser = testSubject.getUser(newUser.getId());
		assertNotNull(newUser);
		assertEquals("dummy", newUser.getUsername());
		assertEquals(1, newUser.getWorkingTimes().size());
		assertTrue(newUser.getWorkingTimes().stream().anyMatch(ww -> "workl".equals(ww.getLabel())));

		testSubject.cloneWorkTimes(newUser.getId(), List.of(3L), List.of(1L, 2L));

		user = testSubject.getUser(3L);
		assertEquals(1, user.getWorkingTimes().size());
		assertTrue(user.getWorkingTimes().stream().anyMatch(ww -> "workl".equals(ww.getLabel())));
	}

	@Test
	public void testChangeStatus() throws ServerException, PersistenceException {
		User user = userDAO.findById(3L);
		assertEquals(1, user.getEnabled());

		testSubject.changeStatus(3L, false);
		user = userDAO.findById(3L);
		assertEquals(0, user.getEnabled());
	}

	@Test
	public void testGenerateAndValidatePassword() throws ServerException, PersistenceException {
		ContextProperties config = Context.get().getProperties();
		String password = testSubject.generatePassword();
		
		final int pwdSize = config.getInt("default" + SecurityServiceImpl.PASSWORD_SIZE, 8);
		final int pwdUpperCase = config.getInt("default" + SecurityServiceImpl.PASSWORD_UPPERCASE, 2);
		final int pwdLowerCase = config.getInt("default" + SecurityServiceImpl.PASSWORD_LOWERCASE, 2);
		final int pwdDigit = config.getInt("default" + SecurityServiceImpl.PASSWORD_DIGIT, 1);
		final int pwdSpecial = config.getInt("default" + SecurityServiceImpl.PASSWORD_SPECIAL, 1);
		final int pwdSequence = config.getInt("default" + SecurityServiceImpl.PASSWORD_SEQUENCE, 4);
		final int pwdOccurrence = config.getInt("default" + SecurityServiceImpl.PASSWORD_OCCURRENCE, 3);
		
		List<String> errors = testSubject.validatePassword(password, pwdSize, pwdUpperCase, pwdLowerCase, pwdDigit,
				pwdSpecial, pwdSequence, pwdOccurrence);
		assertTrue(errors.isEmpty());

		password = testSubject.generatePassword2(pwdSize, pwdUpperCase, pwdLowerCase, pwdDigit, pwdSpecial, pwdSequence,
				pwdOccurrence);
		errors = testSubject.validatePassword(password, pwdSize, pwdUpperCase, pwdLowerCase, pwdDigit, pwdSpecial,
				pwdSequence, pwdOccurrence);
		assertTrue(errors.isEmpty());
	}
}