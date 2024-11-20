package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.DeviceDAO;
import com.logicaldoc.core.security.Geolocation;
import com.logicaldoc.core.security.LoginThrottle;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.core.security.authentication.PasswordAlreadyUsedException;
import com.logicaldoc.core.security.authentication.PasswordWeakException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.core.security.user.WorkingTime;
import com.logicaldoc.core.sequence.Sequence;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.beans.GUIWorkingTime;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.config.WebContextConfigurator;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.security.PasswordCriteria;
import com.logicaldoc.util.security.PasswordGenerator;
import com.logicaldoc.util.security.PasswordValidator;
import com.logicaldoc.util.sql.SqlUtil;
import com.logicaldoc.web.UploadServlet;

/**
 * Implementation of the SecurityService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SecurityServiceImpl extends AbstractRemoteService implements SecurityService {

	private static final String SECURITY_CSP = "security.csp";

	private static final String SECURITY_GEOLOCATION_APIKEY = "security.geolocation.apikey";

	private static final String SSL_REQUIRED = "ssl.required";

	private static final String COOKIES_SECURE = "cookies.secure";

	private static final String COOKIES_SAMESITE = "cookies.samesite";

	private static final String ANONYMOUS_USER = ".anonymous.user";

	private static final String ANONYMOUS_KEY = ".anonymous.key";

	private static final String ANONYMOUS_ENABLED = ".anonymous.enabled";

	private static final String GUI_SAVELOGIN = ".gui.savelogin";

	static final String PASSWORD_OCCURRENCE = ".password.occurrence";

	static final String PASSWORD_SEQUENCE = ".password.sequence";

	static final String PASSWORD_SPECIAL = ".password.special";

	static final String PASSWORD_DIGIT = ".password.digit";

	static final String PASSWORD_LOWERCASE = ".password.lowercase";

	static final String PASSWORD_UPPERCASE = ".password.uppercase";

	static final String PASSWORD_SIZE = ".password.size";

	private static final String ADMIN = "admin";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SecurityServiceImpl.class);

	public static GUITenant getTenant(long tenantId) {
		TenantDAO dao = Context.get().getBean(TenantDAO.class);
		Tenant tenant = null;
		try {
			tenant = dao.findById(tenantId);
			return fromTenant(tenant);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	public static GUITenant fromTenant(Tenant tenant) {
		if (tenant == null)
			return null;
		GUITenant guiTenant = new GUITenant();
		guiTenant.setId(tenant.getId());
		guiTenant.setTenantId(tenant.getTenantId());
		guiTenant.setCity(tenant.getCity());
		guiTenant.setCountry(tenant.getCountry());
		guiTenant.setDisplayName(tenant.getDisplayName());
		guiTenant.setEmail(tenant.getEmail());
		guiTenant.setName(tenant.getName());
		guiTenant.setPostalCode(tenant.getPostalCode());
		guiTenant.setState(tenant.getState());
		guiTenant.setStreet(tenant.getStreet());
		guiTenant.setTelephone(tenant.getTelephone());
		guiTenant.setMaxRepoDocs(tenant.getMaxRepoDocs());
		guiTenant.setMaxRepoSize(tenant.getMaxRepoSize());
		guiTenant.setMaxSessions(tenant.getMaxSessions());
		guiTenant.setQuotaThreshold(tenant.getQuotaThreshold());
		guiTenant.setQuotaAlertRecipients(tenant.getQuotaAlertRecipientsAsList());
		guiTenant.setMaxUsers(tenant.getMaxUsers());
		guiTenant.setMaxGuests(tenant.getMaxGuests());
		guiTenant.setEnabled(tenant.getEnabled() == 1);
		guiTenant.setExpire(tenant.getExpire());

		return guiTenant;
	}

	public static GUITenant getTenant(String tenantName) throws PersistenceException {
		TenantDAO dao = Context.get().getBean(TenantDAO.class);
		Tenant tenant = dao.findByName(tenantName);
		return fromTenant(tenant);
	}

	/**
	 * Used internally by login procedures, instantiates a new GUISession by a
	 * given authenticated user
	 * 
	 * @param session the current session
	 * @param locale the current locale
	 * 
	 * @return session details
	 * 
	 * @throws ServerException a generic error
	 */
	public GUISession loadSession(Session session, String locale) throws ServerException {
		GUISession guiSession = new GUISession();
		guiSession.setSid(session.getSid());
		guiSession.setSingleSignOn(
				session.getDictionary().keySet().stream().anyMatch(k -> k.toLowerCase().contains("saml")));

		DocumentDAO documentDao = Context.get().getBean(DocumentDAO.class);
		SystemMessageDAO messageDao = Context.get().getBean(SystemMessageDAO.class);
		SequenceDAO seqDao = Context.get().getBean(SequenceDAO.class);

		User user = session.getUser();

		GUIUser guiUser = getUser(user.getId());

		if (StringUtils.isEmpty(locale) || "null".equals(locale)) {
			guiUser.setLanguage(user.getLanguage());
			locale = user.getLanguage();
		} else {
			guiUser.setLanguage(locale);
		}

		GUIInfo info = new InfoServiceImpl().getInfo(locale, session.getTenantName(), true);
		guiSession.setInfo(info);

		try {
			guiUser.setPasswordExpired(false);
			guiUser.setLockedDocs(
					documentDao.findByLockUserAndStatus(user.getId(), AbstractDocument.DOC_LOCKED).size());
			guiUser.setCheckedOutDocs(
					documentDao.findByLockUserAndStatus(user.getId(), AbstractDocument.DOC_CHECKED_OUT).size());
			guiUser.setUnreadMessages(messageDao.getUnreadCount(user.getUsername(), Message.TYPE_SYSTEM));
			guiUser.setQuota(user.getQuota());
			guiUser.setQuotaCount(seqDao.getCurrentValue("userquota", user.getId(), user.getTenantId()));
			guiUser.setCertDN(user.getCertDN());
			guiUser.setCertExpire(user.getCertExpire());
			guiUser.setSecondFactor(user.getSecondFactor());

			guiSession.setSid(session.getSid());
			guiSession.setUser(guiUser);
			guiSession.setLoggedIn(true);

			MenuDAO mdao = Context.get().getBean(MenuDAO.class);
			List<Long> menus = mdao.findMenuIdByUserId(session.getUserId(), true);
			guiUser.setMenus(menus);

			loadDashlets(guiUser);

			/*
			 * Prepare an incoming message, if any
			 */
			GenericDAO gDao = Context.get().getBean(GenericDAO.class);
			Generic welcome = gDao.findByAlternateKey("guisetting", "gui.welcome", 0L, session.getTenantId());
			if (welcome != null && StringUtils.isNotEmpty(welcome.getString1())) {
				Map<String, Object> dictionary = new HashMap<>();
				dictionary.put(Automation.LOCALE, user.getLocale());
				dictionary.put(Automation.TENANT_ID, session.getTenantId());
				dictionary.put("session", session);
				dictionary.put("user", guiSession.getUser());

				Automation automation = new Automation("incomingmessage");
				String welcomeMessage = automation.evaluate(welcome.getString1(), dictionary);
				guiSession.setWelcomeMessage(welcomeMessage != null ? welcomeMessage.trim() : null);
			}

			// Define the current locale
			session.getDictionary().put(LOCALE, user.getLocale());
			session.getDictionary().put(USER, user);

			ContextProperties config = Context.get().getProperties();
			guiUser.setPasswordMinLenght(config.getInt(session.getTenantName() + PASSWORD_SIZE, 12));

			return guiSession;
		} catch (PersistenceException | AutomationException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUISession getSession(String locale, String sid) {
		try {
			Session sess = null;
			if (StringUtils.isEmpty(sid))
				sess = validateSession();
			else
				sess = validateSession(sid);
			return loadSession(sess, locale);
		} catch (ServerException e) {
			log.debug(e.getMessage());
			return null;
		}
	}

	@Override
	public void logout() {
		try {
			Session session = validateSession();
			if (session == null)
				return;

			FileUtils.forceDelete(UserUtil.getUserResource(session.getUserId(), "temp"));
			log.info("User {} logged out and closed session {}", session.getUsername(), session.getSid());
			kill(session.getSid());
		} catch (InvalidSessionServerException | IOException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public GUIValue changePassword(Long requestorUserId, long userId, String oldPassword, String newPassword,
			boolean notify) {
		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user == null)
				throw new ServerException(String.format("User %s not found", userId));
			userDao.initialize(user);

			User currentUser = getSessionUser();

			/*
			 * A non admin user cannot change the password of other users
			 */
			MenuDAO mDao = Context.get().getBean(MenuDAO.class);
			if (currentUser != null && currentUser.getId() != userId
					&& !mDao.isReadEnable(Menu.SECURITY, currentUser.getId()))
				throw new PermissionException(String.format("User %s not allowed to change the password of user %s",
						currentUser.getUsername(), user.getUsername()));

			if (oldPassword != null && !CryptUtil.encryptSHA256(oldPassword).equals(user.getPassword())
					&& !CryptUtil.encryptSHA(oldPassword).equals(user.getPassword()))
				throw new ServerException("Wrong old passord");

			UserHistory history = null;
			// The password was changed
			user.setDecodedPassword(newPassword);
			user.setPasswordChanged(new Date());
			user.setRepass("");

			// Add a user history entry
			history = new UserHistory();
			history.setUser(user);
			history.setEvent(UserEvent.PASSWORDCHANGED.toString());
			history.setComment("");

			/*
			 * If the credentials must be notified, we have to mark the password
			 * as expired for security reasons, so the user will change it at
			 * first login.
			 */
			user.setPasswordExpired(notify || requestorUserId == null || !requestorUserId.equals(userId) ? 1 : 0);

			userDao.store(user, history);
			if (notify)
				notifyAccount(user, newPassword);

			return new GUIValue("0", null);
		} catch (PasswordWeakException e) {
			log.error(e.getMessage(), e);
			return new GUIValue("4", e.getMessages().stream().collect(Collectors.joining("\n")));
		} catch (PasswordAlreadyUsedException e) {
			log.error(e.getMessage(), e);
			return new GUIValue("3", e.getFormattedDate());
		} catch (MessagingException e) {
			log.warn(e.getMessage(), e);
			return new GUIValue("2", null);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return new GUIValue("1", null);
		}
	}

	private User getSessionUser() {
		User currentUser = null;
		try {
			currentUser = getSessionUser(getThreadLocalRequest());
		} catch (Exception e) {
			// Do nothing
		}
		return currentUser;
	}

	@Override
	public void addUserToGroup(long groupId, long userId) throws ServerException {
		checkMenu(getThreadLocalRequest(), Menu.SECURITY);
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		GroupDAO groupDao = Context.get().getBean(GroupDAO.class);

		try {
			User user = userDao.findById(userId, true);
			user.addGroup(groupDao.findById(groupId));
			userDao.store(user);
			userDao.initialize(user);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void deleteGroup(long groupId) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
		try {
			Group grp = groupDao.findById(groupId);
			groupDao.initialize(grp);
			for (User user : grp.getUsers()) {
				userDao.initialize(user);
				user.removeGroup(groupId);
				userDao.store(user);
			}
			groupDao.delete(groupId);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void deleteUser(long userId) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);
		UserDAO userDao = Context.get().getBean(UserDAO.class);

		// Create the user history event
		UserHistory transaction = new UserHistory();
		transaction.setSession(session);
		transaction.setEvent(UserEvent.DELETED.toString());
		transaction.setComment("");
		try {
			transaction.setUser(userDao.findById(userId));
			userDao.delete(userId, transaction);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIGroup getGroup(long groupId) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		validateSession();
		GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
		try {
			Group group = groupDao.findById(groupId);

			if (group != null) {
				GUIGroup grp = new GUIGroup();
				grp.setId(groupId);
				grp.setDescription(group.getDescription());
				grp.setName(group.getName());
				grp.setSource(group.getSource());
				return grp;
			}

			return null;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIUser getUser(long userId) throws ServerException {
		Session session = validateSession();

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		SequenceDAO seqDao = Context.get().getBean(SequenceDAO.class);

		try {
			User user = userDao.findById(userId);
			if (user != null) {
				userDao.initialize(user);

				GUIUser guiUser = new GUIUser();
				guiUser.setId(userId);
				guiUser.setTenant(getTenant(user.getTenantId()));
				guiUser.setAddress(user.getStreet());
				guiUser.setCell(user.getTelephone2());
				guiUser.setPhone(user.getTelephone());
				guiUser.setCity(user.getCity());
				guiUser.setCountry(user.getCountry());
				guiUser.setEmail(user.getEmail());
				guiUser.setEmail2(user.getEmail2());
				guiUser.setDepartment(user.getDepartment());
				guiUser.setBuilding(user.getBuilding());
				guiUser.setOrganizationalUnit(user.getOrganizationalUnit());
				guiUser.setCompany(user.getCompany());
				guiUser.setEnabled(user.getEnabled() == 1);
				guiUser.setFirstName(user.getFirstName());
				guiUser.setLanguage(user.getLanguage());
				guiUser.setName(user.getName());
				guiUser.setPostalCode(user.getPostalcode());
				guiUser.setState(user.getState());
				guiUser.setUsername(user.getUsername());
				guiUser.setEvalFormEnabled(user.getEvalFormEnabled() == 1);
				guiUser.setPasswordExpires(user.getPasswordExpires() == 1);
				guiUser.setPasswordExpired(user.getPasswordExpired() == 1);
				guiUser.setWelcomeScreen(user.getWelcomeScreen());
				guiUser.setDefaultWorkspace(user.getDefaultWorkspace());
				guiUser.setIpWhitelist(user.getIpWhiteList());
				guiUser.setIpBlacklist(user.getIpBlackList());
				guiUser.setEmailSignature(user.getEmailSignature());
				guiUser.setEmailSignature2(user.getEmailSignature2());
				guiUser.setCertExpire(user.getCertExpire());
				guiUser.setCertDN(user.getCertDN());
				guiUser.setSecondFactor(user.getSecondFactor());
				guiUser.setKey(user.getKey());
				guiUser.setType(user.getType());
				guiUser.setDocsGrid(user.getDocsGrid());
				guiUser.setHitsGrid(user.getHitsGrid());
				guiUser.setDateFormat(user.getDateFormat());
				guiUser.setDateFormatShort(user.getDateFormatShort());
				guiUser.setDateFormatLong(user.getDateFormatLong());
				guiUser.setSearchPref(user.getSearchPref());
				guiUser.setExpire(user.getExpire());
				guiUser.setEnforceWorkingTime(user.getEnforceWorkingTime() == 1);
				guiUser.setMaxInactivity(user.getMaxInactivity());
				guiUser.setTimeZone(user.getTimeZone());
				guiUser.setSource(user.getSource());
				guiUser.setCreation(user.getCreation());
				guiUser.setLastLogin(user.getLastLogin());

				List<GUIGroup> grps = new ArrayList<>();
				for (Group group : user.getGroups()) {
					GUIGroup guiGroup = new GUIGroup();
					guiGroup.setId(group.getId());
					guiGroup.setName(group.getName());
					guiGroup.setDescription(group.getDescription());
					guiGroup.setType(group.getType());
					guiGroup.setSource(group.getSource());
					grps.add(guiGroup);
				}
				guiUser.setGroups(grps);

				guiUser.setQuota(user.getQuota());

				guiUser.setQuotaCount(seqDao.getCurrentValue("userquota", user.getId(), user.getTenantId()));

				guiUser.setTenant(getTenant(user.getTenantId()));

				ContextProperties config = Context.get().getProperties();
				guiUser.setPasswordMinLenght(config.getInt(guiUser.getTenant().getName() + PASSWORD_SIZE, 12));

				loadDashlets(guiUser);

				loadWorkingTimes(guiUser);

				guiUser.setCustomActions(getMenus(Menu.CUSTOM_ACTIONS, guiUser.getLanguage(), true));

				return guiUser;
			}
		} catch (NumberFormatException | PersistenceException | ServerException e) {
			throwServerException(session, log, e);
		}

		return null;
	}

	/**
	 * Retrieves the dashlets configuration
	 * 
	 * @param usr current user
	 * 
	 * @throws PersistenceException Error in the database
	 */
	protected static void loadDashlets(GUIUser usr) throws PersistenceException {
		DashletServiceImpl dashletService = new DashletServiceImpl();
		UserDAO userDao = Context.get().getBean(UserDAO.class);
		List<GUIDashlet> dashlets = new ArrayList<>();
		Map<String, Generic> map = userDao.findUserSettings(usr.getId(), "dashlet");

		for (Generic generic : map.values()) {
			// This could be a dashlet name or an ID
			String dashletIdentifier = generic.getSubtype().substring(generic.getSubtype().indexOf('-') + 1);
			try {
				GUIDashlet dashlet = null;
				if (StringUtils.isNumeric(dashletIdentifier))
					dashlet = dashletService.get(Long.parseLong(dashletIdentifier));
				else
					dashlet = dashletService.get(dashletIdentifier);

				dashlet.setColumn(generic.getInteger2() != null ? generic.getInteger2().intValue() : 0);
				dashlet.setRow(generic.getInteger3() != null ? generic.getInteger3().intValue() : 0);
				dashlet.setIndex(generic.getString1() != null ? Integer.parseInt(generic.getString1()) : 0);
				dashlets.add(dashlet);
			} catch (NumberFormatException | ServerException e) {
				// Nothing to do
			}
		}
		usr.setDashlets(dashlets);
	}

	@Override
	public void removeFromGroup(long groupId, List<Long> userIds) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
			Group group = groupDao.findById(groupId);
			for (long id : userIds) {
				User user = userDao.findById(id, true);
				user.removeGroup(group.getId());
				userDao.store(user);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIGroup saveGroup(GUIGroup group) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		try {
			GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
			Group grp;
			if (group.getId() != 0) {
				grp = groupDao.findById(group.getId());
				groupDao.initialize(grp);

				grp.setName(group.getName());
				grp.setDescription(group.getDescription());

				if (group.getInheritGroupId() == null || group.getInheritGroupId().longValue() == 0) {
					groupDao.store(grp);
				} else {
					groupDao.insert(grp, group.getInheritGroupId().longValue());
				}
			} else {
				grp = new Group();
				grp.setTenantId(session.getTenantId());
				grp.setName(group.getName());
				grp.setDescription(group.getDescription());

				groupDao.store(grp);

				if (group.getInheritGroupId() != null && group.getInheritGroupId().longValue() != 0)
					groupDao.inheritACLs(grp, group.getInheritGroupId().longValue());
			}

			group.setId(grp.getId());
			return group;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIUser saveUser(GUIUser guiUser, GUIInfo info) throws ServerException {
		Session session = validateSession();

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		boolean createNew = false;

		// Disallow the editing of other users if you do not have access to
		// the Security
		disallowEditingOfOtherUsers(guiUser, session);

		try {
			User user = getOrCreateUser(guiUser);

			createNew = user.getId() == 0L;

			user.setTenantId(session.getTenantId());
			user.setCity(guiUser.getCity());
			user.setCountry(guiUser.getCountry());
			user.setEmail(guiUser.getEmail());
			user.setEmail2(guiUser.getEmail2());
			user.setFirstName(guiUser.getFirstName());
			user.setName(guiUser.getName());
			user.setLanguage(guiUser.getLanguage());
			user.setPostalcode(guiUser.getPostalCode());
			user.setState(guiUser.getState());
			user.setStreet(guiUser.getAddress());
			user.setTelephone(guiUser.getPhone());
			user.setTelephone2(guiUser.getCell());
			user.setBuilding(guiUser.getBuilding());
			user.setOrganizationalUnit(guiUser.getOrganizationalUnit());
			user.setDepartment(guiUser.getDepartment());
			user.setCompany(guiUser.getCompany());
			user.setUsername(guiUser.getUsername());
			user.setEnabled(guiUser.isEnabled() ? 1 : 0);
			user.setPasswordExpires(guiUser.isPasswordExpires() ? 1 : 0);
			user.setPasswordExpired(guiUser.isPasswordExpired() ? 1 : 0);
			user.setEvalFormEnabled(guiUser.isEvalFormEnabled() ? 1 : 0);
			user.setWelcomeScreen(guiUser.getWelcomeScreen());
			user.setIpWhiteList(guiUser.getIpWhitelist());
			user.setIpBlackList(guiUser.getIpBlacklist());
			user.setEmailSignature(guiUser.getEmailSignature());
			user.setDefaultWorkspace(guiUser.getDefaultWorkspace());
			user.setQuota(guiUser.getQuota());
			user.setSecondFactor(StringUtils.isEmpty(guiUser.getSecondFactor()) ? null : guiUser.getSecondFactor());
			user.setKey(guiUser.getKey());
			user.setType(guiUser.getType());
			user.setDocsGrid(guiUser.getDocsGrid());
			user.setHitsGrid(guiUser.getHitsGrid());
			user.setDateFormat(guiUser.getDateFormat());
			user.setDateFormatShort(guiUser.getDateFormatShort());
			user.setDateFormatLong(guiUser.getDateFormatLong());
			user.setSearchPref(guiUser.getSearchPref());
			user.setEnforceWorkingTime(guiUser.isEnforceWorkingTime() ? 1 : 0);
			user.setSecondFactor(guiUser.getSecondFactor());
			user.setMaxInactivity(guiUser.getMaxInactivity() == null || guiUser.getMaxInactivity() == 0 ? null
					: guiUser.getMaxInactivity());
			user.setTimeZone(guiUser.getTimeZone());

			setExpire(user, guiUser);

			if (createNew) {
				User existingUser = userDao.findByUsername(guiUser.getUsername());
				if (existingUser != null) {
					log.warn("Tried to create duplicate username {}", guiUser.getUsername());
					guiUser.setWelcomeScreen(-99);
					return guiUser;
				}

				String tenant = session.getTenantName();

				// Generate an initial password(that must be changed)
				ContextProperties config = Context.get().getProperties();
				String password = PasswordGenerator.generate(config.getInt(tenant + PASSWORD_SIZE, 8),
						config.getInt(tenant + PASSWORD_UPPERCASE, 2), config.getInt(tenant + PASSWORD_LOWERCASE, 2),
						config.getInt(tenant + PASSWORD_DIGIT, 1), config.getInt(tenant + PASSWORD_SPECIAL, 1),
						config.getInt(tenant + PASSWORD_SEQUENCE, 4), config.getInt(tenant + PASSWORD_OCCURRENCE, 3));
				user.setDecodedPassword(password);
				user.setPasswordExpired(1);
				user.setPasswordChanged(new Date());
			}

			saveWorkingTimes(user, guiUser.getWorkingTimes());

			UserHistory transaction = new UserHistory();
			transaction.setSession(session);
			transaction.setEvent(UserEvent.UPDATED.toString());
			userDao.store(user, transaction);

			guiUser.setId(user.getId());

			setGroups(user, guiUser);

			// Notify the user by email
			if (createNew && guiUser.isNotifyCredentials())
				notifyAccount(user, user.getDecodedPassword());
		} catch (MessagingException me) {
			log.warn(me.getMessage(), me);
		} catch (Exception e) {
			return throwServerException(session, log, e);
		}

		return getUser(guiUser.getId());
	}

	private void disallowEditingOfOtherUsers(GUIUser guiUser, Session session)
			throws InvalidSessionServerException, AccessDeniedException {
		if (guiUser.getId() != session.getUserId() && getThreadLocalRequest() != null)
			checkMenu(getThreadLocalRequest(), Menu.SECURITY);
	}

	private void setGroups(User user, GUIUser guiUser) throws PersistenceException {
		UserDAO userDao = Context.get().getBean(UserDAO.class);
		GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
		user.removeGroupMemberships(null);
		for (Long groupId : guiUser.getGroups().stream().map(g -> g.getId()).toList())
			user.addGroup(groupDao.findById(groupId));

		Group adminGroup = groupDao.findByName(ADMIN, user.getTenantId());
		groupDao.initialize(adminGroup);

		// The admin user must be always member of admin group
		if (ADMIN.equals(guiUser.getUsername()) && !guiUser.isMemberOf(Group.GROUP_ADMIN))
			user.addGroup(adminGroup);

		userDao.store(user);
	}

	private void setExpire(User usr, GUIUser guiUser) {
		if (guiUser.getExpire() != null) {
			Calendar cal = Calendar.getInstance();
			cal.setTime(guiUser.getExpire());
			cal.set(Calendar.HOUR_OF_DAY, 23);
			cal.set(Calendar.MINUTE, 59);
			cal.set(Calendar.SECOND, 59);
			cal.set(Calendar.MILLISECOND, 0);
			usr.setExpire(cal.getTime());
		} else
			usr.setExpire(null);
	}

	private User getOrCreateUser(GUIUser guiUser) throws PersistenceException {
		UserDAO userDao = Context.get().getBean(UserDAO.class);
		User usr;
		if (guiUser.getId() != 0) {
			usr = userDao.findById(guiUser.getId());
			userDao.initialize(usr);
		} else {
			usr = new User();
		}
		return usr;
	}

	private void saveWorkingTimes(User user, List<GUIWorkingTime> guiWts) {
		user.getWorkingTimes().clear();
		Calendar cal = Calendar.getInstance();
		for (GUIWorkingTime guiWorkingTime : guiWts) {
			cal.setTime(guiWorkingTime.getStart());
			int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK);
			int hourStart = cal.get(Calendar.HOUR_OF_DAY);
			int minuteStart = cal.get(Calendar.MINUTE);
			WorkingTime wt = new WorkingTime(dayOfWeek, hourStart, minuteStart);

			cal.setTime(guiWorkingTime.getEnd());
			wt.setHourEnd(cal.get(Calendar.HOUR_OF_DAY));
			wt.setMinuteEnd(cal.get(Calendar.MINUTE));

			wt.setLabel(guiWorkingTime.getLabel());
			wt.setDescription(guiWorkingTime.getDescription());

			user.getWorkingTimes().add(wt);
		}
	}

	private void loadWorkingTimes(GUIUser guiUser) throws PersistenceException {
		UserDAO userDao = Context.get().getBean(UserDAO.class);
		User user = userDao.findById(guiUser.getId());
		if (user == null)
			return;
		else
			userDao.initialize(user);

		List<GUIWorkingTime> guiWts = new ArrayList<>();
		if (user.getWorkingTimes() != null)
			for (WorkingTime workingTime : user.getWorkingTimes()) {
				int dayOfWeek = workingTime.getDayOfWeek();

				Calendar cal = Calendar.getInstance();
				// Set the calendar to the last Sunday
				cal.add(Calendar.DAY_OF_WEEK, -(cal.get(Calendar.DAY_OF_WEEK) - 1));

				// Now shift the day of week of the working time
				cal.add(Calendar.DAY_OF_WEEK, dayOfWeek - 1);
				cal.set(Calendar.HOUR_OF_DAY, workingTime.getHourStart());
				cal.set(Calendar.MINUTE, workingTime.getMinuteStart());
				cal.set(Calendar.SECOND, 0);
				cal.set(Calendar.MILLISECOND, 99);
				Date start = cal.getTime();

				cal.set(Calendar.HOUR_OF_DAY, workingTime.getHourEnd());
				cal.set(Calendar.MINUTE, workingTime.getMinuteEnd());
				cal.set(Calendar.SECOND, 0);
				cal.set(Calendar.MILLISECOND, 99);
				Date end = cal.getTime();

				GUIWorkingTime guiWt = new GUIWorkingTime(workingTime.getLabel(), start, end);
				guiWt.setDescription(workingTime.getDescription());

				guiWts.add(guiWt);
			}

		guiUser.setWorkingTimes(guiWts);
	}

	/**
	 * Notify the user with it's new account
	 * 
	 * @param user The created user
	 * @param password The decoded password
	 * 
	 * @throws MessagingException Cannot notify user
	 * @throws AutomationException the script has been evaluated but produced an
	 *         error
	 */
	private void notifyAccount(User user, String password) throws MessagingException, AutomationException {
		EMail email;
		email = new EMail();
		email.setHtml(1);
		Recipient recipient = new Recipient();
		recipient.setAddress(user.getEmail());
		recipient.setRead(1);
		email.addRecipient(recipient);
		email.setFolder("outbox");
		email.setUsername(user.getUsername());
		email.setSentDate(new Date());

		Locale locale = user.getLocale();
		email.setLocale(locale);

		/*
		 * Prepare the template
		 */
		Map<String, Object> dictionary = new HashMap<>();
		ContextProperties config = Context.get().getProperties();
		String address = config.getProperty("server.url");
		dictionary.put("url", address);
		dictionary.put("user", user);
		dictionary.put("password", password);
		dictionary.put(Automation.LOCALE, locale);

		new EMailSender(user.getTenantId()).send(email, "psw.rec1", dictionary);
	}

	@Override
	public GUIUser saveProfile(GUIUser guiUser) throws ServerException {
		Session session = validateSession();

		UserDAO userDao = Context.get().getBean(UserDAO.class);

		// Disallow the editing of other users if you do not have access to
		// the Security
		if (guiUser.getId() != session.getUserId())
			checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		try {
			User user = userDao.findById(guiUser.getId());
			userDao.initialize(user);

			user.setFirstName(guiUser.getFirstName());
			user.setName(guiUser.getName());
			user.setEmail(guiUser.getEmail());
			user.setEmail2(guiUser.getEmail2());
			user.setLanguage(guiUser.getLanguage());
			user.setStreet(guiUser.getAddress());
			user.setPostalcode(guiUser.getPostalCode());
			user.setCity(guiUser.getCity());
			user.setCountry(guiUser.getCountry());
			user.setState(guiUser.getState());
			user.setTelephone(guiUser.getPhone());
			user.setTelephone2(guiUser.getCell());
			user.setCompany(guiUser.getCompany());
			user.setBuilding(guiUser.getBuilding());
			user.setOrganizationalUnit(guiUser.getOrganizationalUnit());
			user.setDepartment(guiUser.getDepartment());

			user.setWelcomeScreen(guiUser.getWelcomeScreen());
			user.setDefaultWorkspace(guiUser.getDefaultWorkspace());
			user.setEmailSignature(guiUser.getEmailSignature());
			user.setEmailSignature2(guiUser.getEmailSignature2());
			user.setTimeZone(guiUser.getTimeZone());
			user.setDocsGrid(guiUser.getDocsGrid());
			user.setHitsGrid(guiUser.getHitsGrid());

			user.setDateFormat(guiUser.getDateFormat());
			user.setDateFormatShort(guiUser.getDateFormatShort());
			user.setDateFormatLong(guiUser.getDateFormatLong());
			user.setSearchPref(guiUser.getSearchPref());

			user.setEvalFormEnabled(guiUser.isEvalFormEnabled() ? 1 : 0);

			UserHistory transaction = new UserHistory();
			transaction.setSession(session);
			transaction.setEvent(UserEvent.UPDATED.toString());
			userDao.store(user, transaction);

			return guiUser;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIUser saveInterfaceSettings(GUIUser user) throws ServerException {
		Session session = validateSession();

		UserDAO userDao = Context.get().getBean(UserDAO.class);

		// Disallow the editing of other users if you do not have access to
		// the Security
		if (user.getId() != session.getUserId())
			checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		try {
			User usr = userDao.findById(user.getId());
			userDao.initialize(usr);

			usr.setWelcomeScreen(user.getWelcomeScreen());
			usr.setDefaultWorkspace(user.getDefaultWorkspace());
			usr.setDocsGrid(user.getDocsGrid());
			usr.setHitsGrid(user.getHitsGrid());

			userDao.store(usr);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

		return user;
	}

	@Override
	public void kill(String sid) {
		// Kill the LogicalDOC session
		SessionManager.get().kill(sid);

		// Also kill the servlet container session, if any
		HttpSession httpSession = SessionManager.get().getServletSession(sid);

		try {
			if (httpSession != null)
				httpSession.invalidate();
		} catch (Exception e) {
			// Ignore
		}

		SessionManager.get().removeSid(getThreadLocalRequest());
	}

	@Override
	public GUISecuritySettings loadSettings() throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		GUISecuritySettings securitySettings = new GUISecuritySettings();

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		ContextProperties pbean = Context.get().getProperties();

		securitySettings.setPwdExpiration(pbean.getInt(session.getTenantName() + ".password.ttl", 90));
		securitySettings.setPwdSize(pbean.getInt(session.getTenantName() + PASSWORD_SIZE, 8));
		securitySettings.setPwdLowerCase(pbean.getInt(session.getTenantName() + PASSWORD_LOWERCASE, 2));
		securitySettings.setPwdUpperCase(pbean.getInt(session.getTenantName() + PASSWORD_UPPERCASE, 2));
		securitySettings.setPwdDigit(pbean.getInt(session.getTenantName() + PASSWORD_DIGIT, 1));
		securitySettings.setPwdSpecial(pbean.getInt(session.getTenantName() + PASSWORD_SPECIAL, 1));
		securitySettings.setPwdSequence(pbean.getInt(session.getTenantName() + PASSWORD_SEQUENCE, 3));
		securitySettings.setPwdOccurrence(pbean.getInt(session.getTenantName() + PASSWORD_OCCURRENCE, 3));
		securitySettings.setPwdEnforceHistory(pbean.getInt(session.getTenantName() + ".password.enforcehistory", 3));
		securitySettings.setMaxInactivity(pbean.getInt(session.getTenantName() + ".security.user.maxinactivity"));
		if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + GUI_SAVELOGIN)))
			securitySettings.setSaveLogin("true".equals(pbean.getProperty(session.getTenantName() + GUI_SAVELOGIN)));
		securitySettings.setIgnoreLoginCase("true".equals(pbean.getProperty("login.ignorecase")));
		securitySettings.setAllowSidInRequest(pbean.getBoolean("security.acceptsid", false));
		securitySettings.setAllowClientId(pbean.getBoolean("security.useclientid", true));
		if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ANONYMOUS_ENABLED)))
			securitySettings.setEnableAnonymousLogin(
					"true".equals(pbean.getProperty(session.getTenantName() + ANONYMOUS_ENABLED)));
		if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ANONYMOUS_KEY)))
			securitySettings.setAnonymousKey(pbean.getProperty(session.getTenantName() + ANONYMOUS_KEY));
		if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ANONYMOUS_USER))) {
			try {
				User user = userDao.findByUsername(pbean.getProperty(session.getTenantName() + ANONYMOUS_USER));
				if (user != null)
					securitySettings.setAnonymousUser(getUser(user.getId()));
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
		if (StringUtils.isNotEmpty(pbean.getProperty(SSL_REQUIRED)))
			securitySettings.setForceSsl("true".equals(pbean.getProperty(SSL_REQUIRED)));
		if (StringUtils.isNotEmpty(pbean.getProperty(COOKIES_SECURE)))
			securitySettings.setCookiesSecure("true".equals(pbean.getProperty(COOKIES_SECURE)));
		securitySettings.setCookiesSameSite(pbean.getProperty(COOKIES_SAMESITE, "unset"));

		securitySettings.setAlertNewDevice(pbean.getBoolean(session.getTenantName() + ".alertnewdevice", true));

		securitySettings.setGeolocationEnabled(pbean.getBoolean("security.geolocation.enabled", true));
		securitySettings.setGeolocationCache(pbean.getBoolean("security.geolocation.cache", false));
		securitySettings.setGeolocationKey(pbean.getProperty(SECURITY_GEOLOCATION_APIKEY));
		securitySettings.setGeolocationDbVer(Geolocation.get().getDatabaseVersion());

		securitySettings.setContentSecurityPolicy(pbean.getProperty(SECURITY_CSP));

		log.debug("Security settings data loaded successfully.");

		return securitySettings;
	}

	@Override
	public boolean saveSettings(GUISecuritySettings settings) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		boolean restartRequired = false;

		ContextProperties conf = Context.get().getProperties();

		if (session.getTenantId() == Tenant.DEFAULT_ID) {
			conf.setProperty("login.ignorecase", Boolean.toString(settings.isIgnoreLoginCase()));
			conf.setProperty(SSL_REQUIRED, Boolean.toString(settings.isForceSsl()));
			conf.setProperty(COOKIES_SECURE, Boolean.toString(settings.isCookiesSecure()));
			conf.setProperty("security.acceptsid", Boolean.toString(settings.isAllowSidInRequest()));
			conf.setProperty("security.useclientid", Boolean.toString(settings.isAllowClientId()));

			conf.setProperty("security.geolocation.enabled", Boolean.toString(settings.isGeolocationEnabled()));
			conf.setProperty("security.geolocation.cache", Boolean.toString(settings.isGeolocationCache()));
			conf.setProperty(SECURITY_GEOLOCATION_APIKEY,
					settings.getGeolocationKey() != null ? settings.getGeolocationKey() : "");

			String currentCsp = conf.getProperty(SECURITY_CSP, "");
			restartRequired = currentCsp.equals(settings.getContentSecurityPolicy());
			conf.setProperty(SECURITY_CSP, settings.getContentSecurityPolicy());

			try {
				// Update the WEB-INF/web.xml
				ServletContext context = getServletContext();
				String policy = "true".equals(conf.getProperty(SSL_REQUIRED)) ? "CONFIDENTIAL" : "NONE";
				WebConfigurator webConfigurator = new WebConfigurator(context.getRealPath("/WEB-INF/web.xml"));
				restartRequired = restartRequired || webConfigurator.setTransportGuarantee(policy);

				// Update the META-INF/context.xml
				conf.setProperty(COOKIES_SAMESITE, settings.getCookiesSameSite());
				WebContextConfigurator webContextConfigurator = new WebContextConfigurator(
						context.getRealPath("/META-INF/context.xml"));
				restartRequired = restartRequired
						|| webContextConfigurator.setSameSiteCookies(settings.getCookiesSameSite());
			} catch (Exception e) {
				log.warn(e.getMessage(), e);
			}

			// Invalidate then Geolocation
			Geolocation.get().dispose();
		}

		conf.setProperty(session.getTenantName() + ".password.ttl", Integer.toString(settings.getPwdExpiration()));
		conf.setProperty(session.getTenantName() + ".security.user.maxinactivity",
				settings.getMaxInactivity() == null || settings.getMaxInactivity().intValue() <= 0 ? ""
						: Integer.toString(settings.getMaxInactivity()));
		conf.setProperty(session.getTenantName() + ".password.enforcehistory",
				Integer.toString(settings.getPwdEnforceHistory()));
		conf.setProperty(session.getTenantName() + PASSWORD_SIZE, Integer.toString(settings.getPwdSize()));
		conf.setProperty(session.getTenantName() + PASSWORD_LOWERCASE, Integer.toString(settings.getPwdLowerCase()));
		conf.setProperty(session.getTenantName() + PASSWORD_UPPERCASE, Integer.toString(settings.getPwdUpperCase()));
		conf.setProperty(session.getTenantName() + PASSWORD_DIGIT, Integer.toString(settings.getPwdDigit()));
		conf.setProperty(session.getTenantName() + PASSWORD_SPECIAL, Integer.toString(settings.getPwdSpecial()));
		conf.setProperty(session.getTenantName() + PASSWORD_SEQUENCE, Integer.toString(settings.getPwdSequence()));
		conf.setProperty(session.getTenantName() + PASSWORD_OCCURRENCE, Integer.toString(settings.getPwdOccurrence()));
		conf.setProperty(session.getTenantName() + GUI_SAVELOGIN, Boolean.toString(settings.isSaveLogin()));
		conf.setProperty(session.getTenantName() + ".alertnewdevice", Boolean.toString(settings.isAlertNewDevice()));
		conf.setProperty(session.getTenantName() + ANONYMOUS_ENABLED,
				Boolean.toString(settings.isEnableAnonymousLogin()));
		conf.setProperty(session.getTenantName() + ANONYMOUS_KEY, settings.getAnonymousKey().trim());

		if (settings.getAnonymousUser() != null)
			conf.setProperty(session.getTenantName() + ANONYMOUS_USER, settings.getAnonymousUser().getUsername());

		try {
			conf.write();
			log.info("Security settings data written successfully.");
			return restartRequired;
		} catch (IOException e) {
			return throwServerException(session, log, e);
		}
	}

	private void saveACL(Session session, Menu menu, List<GUIAccessControlEntry> aces)
			throws PermissionException, PersistenceException {
		MenuDAO mdao = Context.get().getBean(MenuDAO.class);
		if (!mdao.isReadEnable(Menu.SECURITY, session.getUserId()))
			throw new PermissionException(session.getUsername(), "Menu " + menu.getName(), Permission.READ);

		GroupDAO gdao = Context.get().getBean(GroupDAO.class);
		mdao.initialize(menu);

		// Remove all current tenant rights
		Set<AccessControlEntry> grps = new HashSet<>();
		for (AccessControlEntry mg : menu.getAccessControlList()) {
			Group group = gdao.findById(mg.getGroupId());
			if (group != null && group.getTenantId() != session.getTenantId())
				grps.add(mg);
		}
		menu.getAccessControlList().clear();

		for (GUIAccessControlEntry right : aces) {
			Group group = gdao.findById(right.getEntityId());
			if (group == null || group.getTenantId() != session.getTenantId())
				continue;

			AccessControlEntry ace = new AccessControlEntry();
			ace.setGroupId(right.getEntityId());
			ace.setRead(right.isRead() ? 1 : 0);
			ace.setWrite(right.isWrite() ? 1 : 0);
			grps.add(ace);
		}

		menu.setAccessControlList(grps);
		mdao.store(menu);
	}

	@Override
	public void saveACL(GUIMenu menu) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);
		MenuDAO mdao = Context.get().getBean(MenuDAO.class);
		try {
			saveACL(session, mdao.findById(menu.getId()), menu.getAccessControlList());
		} catch (PermissionException | PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void deleteMenu(long menuId) throws ServerException {
		Session session = validateSession();

		MenuDAO dao = Context.get().getBean(MenuDAO.class);
		try {
			Menu menu = dao.findById(menuId);
			if (menu == null)
				throw new ServerException("Unexisting menu identified by " + menuId);
			if (menu.getType() == Menu.TYPE_DEFAULT)
				throw new PermissionException("Cannot delete legacy menu " + menuId);
			dao.delete(menuId);
		} catch (PermissionException | PersistenceException | ServerException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void saveMenus(List<GUIMenu> menus, String locale) throws ServerException {
		validateSession();

		for (GUIMenu guiMenu : menus)
			saveMenu(guiMenu, locale);
	}

	@Override
	public GUIMenu saveMenu(GUIMenu guiMenu, String locale) throws ServerException {
		Session session = validateSession();

		MenuDAO dao = Context.get().getBean(MenuDAO.class);
		try {
			Menu menu = new Menu();
			if (guiMenu.getId() != 0L) {
				menu = dao.findById(guiMenu.getId());
				dao.initialize(menu);
			} else {
				menu.setTenantId(session.getTenantId());
			}

			menu.setName(guiMenu.getName().replace("/", ""));
			menu.setAutomation(guiMenu.getAutomation());
			menu.setEnabled(guiMenu.isEnabled() ? 1 : 0);
			menu.setDescription(guiMenu.getDescription());
			menu.setPosition(guiMenu.getPosition());
			menu.setParentId(guiMenu.getParentId());
			menu.setRoutineId(guiMenu.getRoutineId());
			menu.setType(guiMenu.getType());

			menu.getAccessControlList().clear();
			for (GUIAccessControlEntry right : guiMenu.getAccessControlList())
				menu.getAccessControlList().add(new AccessControlEntry(right.getEntityId()));

			dao.store(menu);
			return getMenu(menu.getId(), locale);
		} catch (PersistenceException | ServerException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public List<GUIMenu> getMenus(long parentId, String locale, boolean enabledOnly) throws ServerException {
		Session session = validateSession();

		MenuDAO dao = Context.get().getBean(MenuDAO.class);

		List<Menu> menus = dao.findByUserId(session.getUserId(), parentId, enabledOnly);
		List<GUIMenu> guiMenus = menus.stream().filter(m -> m.getTenantId() == session.getTenantId()).map(m -> {
			try {
				return toGUIMenu(m, locale);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
				return null;
			}
		}).collect(Collectors.toList());
		guiMenus.sort((m1, m2) -> {
			if (m1.getPosition() == m2.getPosition())
				return m1.getName().compareToIgnoreCase(m2.getName());
			return m1.getPosition() < m2.getPosition() ? -1 : 1;
		});

		return guiMenus;

	}

	@Override
	public GUIMenu getMenu(long menuId, String locale) throws ServerException {
		Session session = validateSession();

		GroupDAO gdao = Context.get().getBean(GroupDAO.class);
		MenuDAO dao = Context.get().getBean(MenuDAO.class);
		try {
			Menu menu = dao.findById(menuId);
			if (menu == null)
				return null;

			GUIMenu f = toGUIMenu(menu, locale);
			List<GUIAccessControlEntry> acl = new ArrayList<>();
			for (AccessControlEntry fg : menu.getAccessControlList()) {
				Group group = gdao.findById(fg.getGroupId());
				if (group == null || group.getTenantId() != session.getTenantId())
					continue;

				GUIAccessControlEntry ace = new GUIAccessControlEntry();
				ace.setEntityId(fg.getGroupId());
				acl.add(ace);
			}
			f.setAccessControlList(acl);
			return f;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}

	}

	private GUIMenu toGUIMenu(Menu menu, String locale) throws PersistenceException {
		GUIMenu f = new GUIMenu();
		f.setId(menu.getId());
		f.setName(menu.getName());
		f.setEnabled(menu.getEnabled() == 1);
		f.setAutomation(menu.getAutomation());
		f.setRoutineId(menu.getRoutineId());
		f.setPosition(menu.getPosition());
		f.setDescription(menu.getDescription());
		f.setParentId(menu.getParentId());
		f.setType(menu.getType());

		MenuDAO dao = Context.get().getBean(MenuDAO.class);
		dao.initialize(menu);

		List<GUIAccessControlEntry> acl = new ArrayList<>();

		GroupDAO gdao = Context.get().getBean(GroupDAO.class);
		UserDAO udao = Context.get().getBean(UserDAO.class);
		for (AccessControlEntry mg : menu.getAccessControlList()) {
			GUIAccessControlEntry ace = new GUIAccessControlEntry();
			ace.setEntityId(mg.getGroupId());
			ace.setWrite(mg.getWrite() == 1);

			Group group = gdao.findById(mg.getGroupId());
			if (group == null)
				continue;

			if (group.getType() == Group.TYPE_DEFAULT) {
				ace.setLabel(group.getName());
				ace.setName(I18N.message("group", LocaleUtil.toLocale(locale)) + ": " + group.getName());
			} else {
				User user = udao.findByGroup(group.getId()).iterator().next();
				ace.setLabel(user.getUsername());
				ace.setName(I18N.message("user", LocaleUtil.toLocale(locale)) + ": " + user.getFullName() + " ("
						+ user.getUsername() + ")");
			}

			acl.add(ace);
		}
		f.setAccessControlList(acl);

		return f;
	}

	@Override
	public List<GUIUser> searchUsers(String username, String groupId) throws ServerException {
		Session session = validateSession();

		UserDAO userDao = Context.get().getBean(UserDAO.class);

		StringBuilder query = new StringBuilder(
				"select A.ld_id, A.ld_username, A.ld_name, A.ld_firstname from ld_user A ");
		if (StringUtils.isNotEmpty(groupId))
			query.append(", ld_usergroup B");
		query.append(" where A.ld_deleted=0 and A.ld_type=" + User.TYPE_DEFAULT);
		if (StringUtils.isNotEmpty(username))
			query.append(" and A.ld_username like '%" + SqlUtil.doubleQuotes(username) + "%'");
		if (StringUtils.isNotEmpty(groupId))
			query.append(" and A.ld_id=B.ld_userid and B.ld_groupid=" + Long.parseLong(groupId));

		try {
			return userDao.query(query.toString(), new RowMapper<>() {

				@Override
				public GUIUser mapRow(ResultSet rs, int row) throws SQLException {
					GUIUser user = new GUIUser();
					user.setId(rs.getLong(1));
					user.setUsername(rs.getString(2));
					user.setName(rs.getString(3));
					user.setFirstName(rs.getString(4));
					return user;
				}
			}, null);
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public List<GUISequence> loadBlockedEntities() throws ServerException {
		Session session = validateSession();
		if (session.getTenantId() != Tenant.DEFAULT_ID)
			return new ArrayList<>();

		ContextProperties config = Context.get().getProperties();
		SequenceDAO dao = Context.get().getBean(SequenceDAO.class);
		List<Sequence> seqs = new ArrayList<>();
		long max = config.getInt("throttle.username.max", 0);
		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.MINUTE, -config.getInt("throttle.username.wait", 0));
		Date oldestDate = cal.getTime();

		Map<String, Object> params = new HashMap<>();
		params.put("oldestDate", oldestDate);
		params.put("max", max);

		final String NAME_CONDITION = "_entity.name like '";
		final String MORE_CONDITIONS = "%' and _entity.value >= :max and _entity.lastModified >= :oldestDate";

		try {
			if (max > 0)
				seqs.addAll(dao.findByWhere(NAME_CONDITION + LoginThrottle.LOGINFAIL_USERNAME + MORE_CONDITIONS, params,
						null, null));

			max = config.getInt("throttle.ip.max", 0);
			cal = Calendar.getInstance();
			cal.add(Calendar.MINUTE, -config.getInt("throttle.ip.wait", 0));
			if (max > 0)
				seqs.addAll(dao.findByWhere(NAME_CONDITION + LoginThrottle.LOGINFAIL_IP + MORE_CONDITIONS, params, null,
						null));

			max = config.getInt("throttle.apikey.max", 0);
			cal = Calendar.getInstance();
			cal.add(Calendar.MINUTE, -config.getInt("throttle.apikey.wait", 0));
			if (max > 0)
				seqs.addAll(dao.findByWhere(NAME_CONDITION + LoginThrottle.LOGINFAIL_APIKEY + MORE_CONDITIONS, params,
						null, null));

			ArrayList<GUISequence> ret = new ArrayList<>();
			for (Sequence seq : seqs) {
				GUISequence guiSeq = new GUISequence();
				guiSeq.setId(seq.getId());
				guiSeq.setValue(seq.getValue());
				guiSeq.setLastModified(seq.getLastModified());
				guiSeq.setName(seq.getName());
				ret.add(guiSeq);
			}
			return ret;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public void removeBlockedEntities(List<Long> ids) throws ServerException {
		Session session = validateSession();
		if (session.getTenantId() != Tenant.DEFAULT_ID)
			return;

		SequenceDAO dao = Context.get().getBean(SequenceDAO.class);
		try {
			for (long id : ids) {
				dao.delete(id);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void replicateUsersSettings(long masterUserId, List<Long> userIds, boolean gui, boolean groups)
			throws ServerException {
		Session session = validateSession();

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		try {
			User masterUser = userDao.findById(masterUserId);
			userDao.initialize(masterUser);
			Set<Group> masterGroups = masterUser.getGroups();

			for (Long userId : userIds) {
				User user = userDao.findById(userId);
				userDao.initialize(user);

				if (gui) {
					user.setDefaultWorkspace(masterUser.getDefaultWorkspace());
					user.setWelcomeScreen(masterUser.getWelcomeScreen());
					user.setDocsGrid(masterUser.getDocsGrid());
					user.setHitsGrid(masterUser.getHitsGrid());
				}

				if (groups && !user.isReadonly()) {
					user.removeGroupMemberships(null);
					for (Group grp : masterGroups) {
						if (!grp.isUserGroup())
							user.addGroup(grp);
					}
				}

				UserHistory transaction = new UserHistory();
				transaction.setSession(session);
				transaction.setComment(String.format("Settings replicated from %s", masterUser.getUsername()));
				userDao.store(user, transaction);

				log.info("Replicated the settings to user {}", user.getName());
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void updateDeviceLabel(long deviceId, String label) throws ServerException {
		Session session = validateSession();
		DeviceDAO dDao = Context.get().getBean(DeviceDAO.class);
		try {
			Device device = dDao.findById(deviceId);
			if (device != null) {
				device.setLabel(StringUtils.isNotEmpty(label) ? label.trim() : null);
				dDao.store(device);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public String trustDevice(String label) throws ServerException {
		Session session = validateSession();
		if (session.getClient() != null && session.getClient().getDevice() != null) {
			Device device = session.getClient().getDevice();
			if (label != null)
				device.setLabel(label);
			DeviceDAO dDao = Context.get().getBean(DeviceDAO.class);
			try {
				device = dDao.trustDevice(session.getUser(), device);
			} catch (PersistenceException e) {
				return throwServerException(session, log, e);
			}
			return device.getDeviceId();
		} else
			return null;

	}

	@Override
	public Boolean isTrustedDevice(String deviceId) throws ServerException {
		Session session = validateSession();
		// If the second factor is not enabled on the user, the device is
		// always trusted
		if (StringUtils.isEmpty(session.getUser().getSecondFactor()))
			return true;

		DeviceDAO dDao = Context.get().getBean(DeviceDAO.class);
		Device device = dDao.findByDeviceId(deviceId);
		return device != null && device.getUserId() == session.getUserId() && device.getTrusted() == 1;

	}

	@Override
	public void deleteTrustedDevices(List<Long> ids) throws ServerException {
		Session session = validateSession();
		DeviceDAO dDao = Context.get().getBean(DeviceDAO.class);
		for (Long id : ids)
			try {
				dDao.delete(id);
			} catch (NumberFormatException | PersistenceException e) {
				throwServerException(session, log, e);
			}
	}

	@Override
	public String syncGeolocationDB(String key) throws ServerException {
		Session session = validateSession();
		Context.get().getProperties().setProperty(SECURITY_GEOLOCATION_APIKEY, key != null ? key : "");
		try {
			Context.get().getProperties().write();

			Geolocation.get().syncDB(key);
			return Geolocation.get().getDatabaseVersion();
		} catch (IOException e) {
			return throwServerException(session, log, e);
		}

	}

	@Override
	public void saveAvatar(long userId) throws ServerException {
		Session session = validateSession();

		Map<String, File> uploadedFilesMap = UploadServlet.getUploads(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user != null)
				UserUtil.saveAvatar(user, file);
		} catch (PersistenceException e) {
			log.error("Unable to store the avatar", e);
		} finally {
			UploadServlet.cleanUploads(session.getSid());
		}
	}

	@Override
	public void resetAvatar(long userId) throws ServerException {
		Session session = validateSession();

		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user != null)
				UserUtil.generateDefaultAvatar(user);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void cloneWorkTimes(long srcUserId, List<Long> userIds, List<Long> groupIds) throws ServerException {
		Session session = validateSession();

		Set<Long> uniqueUserIds = userIds.stream().distinct().collect(Collectors.toSet());

		if (groupIds != null) {
			UserDAO gDao = Context.get().getBean(UserDAO.class);
			groupIds.stream().forEach(gId -> {
				try {
					Set<User> usrs = gDao.findByGroup(gId);
					for (User user : usrs)
						uniqueUserIds.add(user.getId());
				} catch (PersistenceException e) {
					log.error(e.getMessage(), e);
				}
			});
		}

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		try {
			User srcUser = userDao.findById(srcUserId);
			userDao.initialize(srcUser);

			for (Long userId : uniqueUserIds) {
				User user = userDao.findById(userId);
				userDao.initialize(user);
				if (srcUser.getWorkingTimes() != null)
					for (WorkingTime wt : srcUser.getWorkingTimes())
						user.getWorkingTimes().add(new WorkingTime(wt));
				UserHistory transaction = new UserHistory();
				transaction.setSession(session);
				transaction.setEvent(UserEvent.UPDATED.toString());
				userDao.store(user, transaction);
			}
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}

	}

	@Override
	public void changeStatus(long userId, boolean enabled) throws ServerException {
		checkMenu(getThreadLocalRequest(), Menu.SECURITY);
		Session session = checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		UserDAO userDao = Context.get().getBean(UserDAO.class);
		try {
			User user = userDao.findById(userId, true);
			if (user == null)
				throw new ServerException(String.format("User %s not found", userId));

			if (!enabled && ADMIN.equals(user.getUsername()))
				throw new ServerException("Cannot diable the admin user");

			UserHistory transaction = new UserHistory();
			transaction.setSession(session);
			transaction.setEvent(enabled ? UserEvent.UPDATED.toString() : UserEvent.DISABLED.toString());

			user.setEnabled(enabled ? 1 : 0);
			userDao.store(user, transaction);
		} catch (PersistenceException | ServerException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public String generatePassword() throws InvalidSessionServerException {
		Session session = validateSession();
		String tenant = session.getTenantName();

		// Generate an initial password(that must be changed)
		ContextProperties config = Context.get().getProperties();
		return PasswordGenerator.generate(config.getInt(tenant + PASSWORD_SIZE, 8),
				config.getInt(tenant + PASSWORD_UPPERCASE, 2), config.getInt(tenant + PASSWORD_LOWERCASE, 2),
				config.getInt(tenant + PASSWORD_DIGIT, 1), config.getInt(tenant + PASSWORD_SPECIAL, 1),
				config.getInt(tenant + PASSWORD_SEQUENCE, 4), config.getInt(tenant + PASSWORD_OCCURRENCE, 3));
	}

	@Override
	public String generatePassword2(int length, int uppercaseChars, int lowercaseChars, int digits, int specialChars,
			int maxSequenceSize, int maxOccurrences) {
		return PasswordGenerator.generate(length, uppercaseChars, lowercaseChars, digits, specialChars, maxSequenceSize,
				maxOccurrences);
	}

	@Override
	public List<String> validatePassword(String password, int minLength, int uppercaseChars, int lowercaseChars,
			int digits, int specialChars, int maxSequenceSize, int maxOccurrences) {
		PasswordCriteria criteria = new PasswordCriteria(minLength, uppercaseChars, lowercaseChars, digits,
				specialChars);
		criteria.setMaxSequenceSize(maxSequenceSize);
		criteria.setMaxOccurrences(maxOccurrences);

		PasswordValidator validator = new PasswordValidator(criteria, null);
		return validator.validate(password);
	}

	@Override
	public String createApiKey(String name) throws ServerException {
		Session session = validateSession();

		try {
			ApiKeyDAO dao = Context.get().getBean(ApiKeyDAO.class);
			ApiKey apiKey = dao.findByName(name, session.getUserId());
			if (apiKey != null)
				throw new ServerException("A key with same name already exists");

			apiKey = new ApiKey(session.getUserId(), name);
			dao.store(apiKey);

			UserHistory transaction = new UserHistory();
			transaction.setSession(session);
			transaction.setComment(name + " (" + apiKey.getLabel() + ")");
			transaction.setEvent(UserEvent.NEWAPIKEY.toString());

			UserHistoryDAO historyDao = Context.get().getBean(UserHistoryDAO.class);
			historyDao.store(transaction);

			return apiKey.getDecodedKey();
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void deleteApiKey(long keyId) throws ServerException {
		Session session = validateSession();

		try {
			ApiKeyDAO dao = Context.get().getBean(ApiKeyDAO.class);
			dao.delete(keyId);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void updateApiKey(long keyId, String newName) throws ServerException {
		Session session = validateSession();

		try {
			ApiKeyDAO dao = Context.get().getBean(ApiKeyDAO.class);
			ApiKey apiKey = dao.findById(keyId);
			apiKey.setName(newName);
			dao.store(apiKey);
		} catch (PersistenceException e) {
			throwServerException(session, log, e);
		}
	}
}