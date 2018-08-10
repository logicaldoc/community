package com.logicaldoc.web.service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.text.StrSubstitutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.ibm.icu.util.Calendar;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.rss.dao.FeedMessageDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.LoginThrottle;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.MenuGroup;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.sequence.Sequence;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIExternalCall;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.security.PasswordGenerator;
import com.logicaldoc.util.sql.SqlUtil;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the SecurityService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SecurityServiceImpl extends RemoteServiceServlet implements SecurityService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SecurityServiceImpl.class);

	public static GUITenant getTenant(long tenantId) {
		TenantDAO dao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant tenant = dao.findById(tenantId);
		return fromTenant(tenant);
	}

	public static GUITenant fromTenant(Tenant tenant) {
		if (tenant == null)
			return null;
		GUITenant ten = new GUITenant();
		ten.setId(tenant.getId());
		ten.setTenantId(tenant.getTenantId());
		ten.setCity(tenant.getCity());
		ten.setCountry(tenant.getCountry());
		ten.setDisplayName(tenant.getDisplayName());
		ten.setEmail(tenant.getEmail());
		ten.setName(tenant.getName());
		ten.setPostalCode(tenant.getPostalCode());
		ten.setState(tenant.getState());
		ten.setStreet(tenant.getStreet());
		ten.setTelephone(tenant.getTelephone());
		ten.setMaxRepoDocs(tenant.getMaxRepoDocs());
		ten.setMaxRepoSize(tenant.getMaxRepoSize());
		ten.setMaxSessions(tenant.getMaxSessions());
		ten.setQuotaThreshold(tenant.getQuotaThreshold());
		ten.setQuotaAlertRecipients(tenant.getQuotaAlertRecipientsAsList().toArray(new String[0]));
		ten.setMaxUsers(tenant.getMaxUsers());
		ten.setEnabled(tenant.getEnabled() == 1);
		ten.setExpire(tenant.getExpire());

		return ten;
	}

	public static GUITenant getTenant(String tenantName) {
		TenantDAO dao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant tenant = dao.findByName(tenantName);
		return fromTenant(tenant);
	}

	/**
	 * Used internally by login procedures, instantiates a new GUISession by a
	 * given authenticated user.
	 */
	public GUISession loadSession(Session sess, String locale) {
		try {
			GUIUser guiUser = new GUIUser();
			GUISession session = new GUISession();
			session.setSid(sess.getSid());

			DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			SystemMessageDAO messageDao = (SystemMessageDAO) Context.get().getBean(SystemMessageDAO.class);
			SequenceDAO seqDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);

			User user = sess.getUser();

			guiUser.setFirstName(user.getFirstName());
			guiUser.setName(user.getName());
			guiUser.setId(user.getId());
			guiUser.setTenantId(user.getTenantId());
			if (StringUtils.isEmpty(locale) || "null".equals(locale)) {
				guiUser.setLanguage(user.getLanguage());
			} else {
				guiUser.setLanguage(locale);
			}

			GUIInfo info = new InfoServiceImpl().getInfo(guiUser.getLanguage(), sess.getTenantName());
			session.setInfo(info);

			guiUser.setName(user.getName());
			guiUser.setEmail(user.getEmail());
			guiUser.setEmail2(user.getEmail2());
			guiUser.setEmailSignature(user.getEmailSignature());
			guiUser.setEmailSignature2(user.getEmailSignature2());

			GUIGroup[] groups = new GUIGroup[user.getGroups().size()];
			int i = 0;
			for (Group g : user.getGroups()) {
				groups[i] = new GUIGroup();
				groups[i].setId(g.getId());
				groups[i].setName(g.getName());
				groups[i].setDescription(g.getDescription());
				i++;
			}
			guiUser.setGroups(groups);

			guiUser.setUserName(user.getUsername());
			guiUser.setPasswordExpired(false);

			guiUser.setLockedDocs(documentDao.findByLockUserAndStatus(user.getId(), AbstractDocument.DOC_LOCKED).size());
			guiUser.setCheckedOutDocs(documentDao.findByLockUserAndStatus(user.getId(),
					AbstractDocument.DOC_CHECKED_OUT).size());
			guiUser.setUnreadMessages(messageDao.getCount(user.getUsername(), SystemMessage.TYPE_SYSTEM, 0));

			guiUser.setQuota(user.getQuota());
			guiUser.setQuotaCount(seqDao.getCurrentValue("userquota", user.getId(), user.getTenantId()));
			guiUser.setWelcomeScreen(user.getWelcomeScreen());
			guiUser.setDefaultWorkspace(user.getDefaultWorkspace());
			guiUser.setCertDN(user.getCertDN());
			guiUser.setCertExpire(user.getCertExpire());
			guiUser.setSecondFactor(user.getSecondFactor());

			session.setSid(sess.getSid());
			session.setUser(guiUser);
			session.setLoggedIn(true);

			MenuDAO mdao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			List<Long> menues = mdao.findMenuIdByUserId(sess.getUserId());
			guiUser.setMenues((Long[]) menues.toArray(new Long[0]));

			loadDashlets(guiUser);

			/*
			 * Prepare an incoming message, if any
			 */
			ContextProperties config = Context.get().getProperties();
			String incomingMessage = config.getProperty(sess.getTenantName() + ".gui.welcome");
			if (StringUtils.isNotEmpty(incomingMessage)) {
				Map<String, String> map = new HashMap<String, String>();
				map.put("user", user.getFullName());
				incomingMessage = StrSubstitutor.replace(incomingMessage, map);
			}

			// In case of news overwrite the incoming message
			if (guiUser.isMemberOf(Constants.GROUP_ADMIN) && info.isEnabled("Feature_27")) {
				// Check if there are incoming messages not already read
				FeedMessageDAO feedMessageDao = (FeedMessageDAO) Context.get().getBean(FeedMessageDAO.class);
				if (feedMessageDao.checkNotRead())
					incomingMessage = I18N.message("productnewsmessage", user.getLocale());
			}

			if (StringUtils.isNotEmpty(incomingMessage))
				session.setIncomingMessage(incomingMessage);

			// Define the current locale
			sess.getDictionary().put(ServiceUtil.LOCALE, user.getLocale());
			sess.getDictionary().put(ServiceUtil.USER, user);

			guiUser.setPasswordMinLenght(Integer.parseInt(config.getProperty(sess.getTenantName() + ".password.size")));

			/*
			 * Prepare the external command
			 */
			String tenant = sess.getTenantName();
			if (info.isEnabled("Feature_31") && "true".equals(config.getProperty(tenant + ".extcall.enabled"))) {
				GUIExternalCall externalCall = new GUIExternalCall();
				externalCall.setName(config.getProperty(tenant + ".extcall.name"));
				externalCall.setBaseUrl(config.getProperty(tenant + ".extcall.baseurl"));
				externalCall.setSuffix(config.getProperty(tenant + ".extcall.suffix"));
				externalCall.setTargetWindow(config.getProperty(tenant + ".extcall.window"));
				externalCall.setParametersStr(config.getProperty(tenant + ".extcall.params"));
				session.setExternalCall(externalCall);
			}

			return session;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	@Override
	public GUISession getSession(String locale) {
		try {
			Session sess = ServiceUtil.validateSession(getThreadLocalRequest());

			GUISession session = loadSession(sess, locale);
			return session;
		} catch (Throwable e) {
			log.debug(e.getMessage());
			return null;
		}
	}

	@Override
	public void logout() {
		try {
			Session session = ServiceUtil.validateSession(getThreadLocalRequest());
			if (session == null)
				return;

			FileUtils.forceDelete(UserUtil.getUserResource(session.getUserId(), "temp"));
			log.info("User " + session.getUsername() + " logged out and closed session " + session.getSid());
			kill(session.getSid());
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public int changePassword(long userId, String oldPassword, String newPassword, boolean notify) {
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user == null)
				throw new Exception(String.format("User %s not found", userId));
			userDao.initialize(user);

			User currentUser = null;
			try {
				currentUser = ServiceUtil.getSessionUser(getThreadLocalRequest());
			} catch (Throwable t) {
			}

			/*
			 * A non admin user cannot change the password of other users
			 */
			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			if (currentUser != null && currentUser.getId() != userId
					&& !mDao.isReadEnable(Menu.SECURITY, currentUser.getId()))
				throw new Exception(String.format("User %s not allowed to change the password of user %s",
						currentUser.getUsername(), user.getUsername()));

			if (oldPassword != null && !CryptUtil.cryptString(oldPassword).equals(user.getPassword()))
				throw new Exception("Wrong old passord");

			UserHistory history = null;
			// The password was changed
			user.setDecodedPassword(newPassword);
			user.setPasswordChanged(new Date());
			user.setPasswordExpired(0);
			user.setRepass("");

			// Add a user history entry
			history = new UserHistory();
			history.setUser(user);
			history.setEvent(UserHistory.EVENT_USER_PASSWORDCHANGED);
			history.setComment("");

			boolean stored = userDao.store(user, history);

			if (!stored)
				throw new Exception("User not stored");

			if (notify)
				try {
					notifyAccount(user, newPassword);
				} catch (Throwable e) {
					log.warn(e.getMessage(), e);
					return 2;
				}
			return 0;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return 1;
		}
	}

	@Override
	public void addUserToGroup(long groupId, long userId) throws ServerException {
		ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		User user = userDao.findById(userId, true);
		user.addGroup(groupDao.findById(groupId));
		userDao.store(user);
		userDao.initialize(user);
	}

	@Override
	public void deleteGroup(long groupId) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			Group grp = groupDao.findById(groupId);
			groupDao.initialize(grp);
			for (User user : grp.getUsers()) {
				user.removeGroup(groupId);
				userDao.store(user);
			}
			groupDao.delete(groupId);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteUser(long userId) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		// Create the user history event
		UserHistory transaction = new UserHistory();
		transaction.setSession(session);
		transaction.setEvent(UserHistory.EVENT_USER_DELETED);
		transaction.setComment("");
		transaction.setUser(userDao.findById(userId));

		userDao.delete(userId, transaction);
	}

	@Override
	public GUIGroup getGroup(long groupId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());
		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		Group group = groupDao.findById(groupId);

		if (group != null) {
			GUIGroup grp = new GUIGroup();
			grp.setId(groupId);
			grp.setDescription(group.getDescription());
			grp.setName(group.getName());
			return grp;
		}

		return null;
	}

	@Override
	public GUIUser getUser(long userId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			SequenceDAO seqDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);

			User user = userDao.findById(userId);
			if (user != null) {
				userDao.initialize(user);

				GUIUser usr = new GUIUser();
				usr.setId(userId);
				usr.setTenantId(user.getTenantId());
				usr.setAddress(user.getStreet());
				usr.setCell(user.getTelephone2());
				usr.setPhone(user.getTelephone());
				usr.setCity(user.getCity());
				usr.setCountry(user.getCountry());
				usr.setEmail(user.getEmail());
				usr.setEmail2(user.getEmail2());
				usr.setEnabled(user.getEnabled() == 1);
				usr.setFirstName(user.getFirstName());
				usr.setLanguage(user.getLanguage());
				usr.setName(user.getName());
				usr.setPostalCode(user.getPostalcode());
				usr.setState(user.getState());
				usr.setUserName(user.getUsername());
				usr.setPasswordExpires(user.getPasswordExpires() == 1);
				usr.setPasswordExpired(user.getPasswordExpired() == 1);
				usr.setWelcomeScreen(user.getWelcomeScreen());
				usr.setDefaultWorkspace(user.getDefaultWorkspace());
				usr.setIpWhitelist(user.getIpWhiteList());
				usr.setIpBlacklist(user.getIpBlackList());
				usr.setEmailSignature(user.getEmailSignature());
				usr.setEmailSignature2(user.getEmailSignature2());
				usr.setCertExpire(user.getCertExpire());
				usr.setCertDN(user.getCertDN());
				usr.setSecondFactor(user.getSecondFactor());
				usr.setKey(user.getKey());

				GUIGroup[] grps = new GUIGroup[user.getGroups().size()];
				int i = 0;
				for (Group group : user.getGroups()) {
					grps[i] = new GUIGroup();
					grps[i].setId(group.getId());
					grps[i].setName(group.getName());
					grps[i].setDescription(group.getDescription());
					grps[i].setType(group.getType());
					i++;
				}
				usr.setGroups(grps);

				usr.setQuota(user.getQuota());

				usr.setQuotaCount(seqDao.getCurrentValue("userquota", user.getId(), user.getTenantId()));

				Tenant tenant = tenantDao.findById(user.getTenantId());

				ContextProperties config = Context.get().getProperties();
				usr.setPasswordMinLenght(Integer.parseInt(config.getProperty(tenant.getName() + ".password.size")));

				loadDashlets(usr);

				return usr;
			}
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
		return null;
	}

	/**
	 * Retrieves the dashlets configuration
	 */
	protected static void loadDashlets(GUIUser usr) {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		List<GUIDashlet> dashlets = new ArrayList<GUIDashlet>();
		Map<String, Generic> map = userDao.findUserSettings(usr.getId(), "dashlet");
		for (Generic generic : map.values()) {
			dashlets.add(new GUIDashlet(generic.getInteger1().intValue(), generic.getInteger2().intValue(), generic
					.getInteger3().intValue(), generic.getString1() != null ? Integer.parseInt(generic.getString1())
					: 0));
		}

		usr.setDashlets(dashlets.toArray(new GUIDashlet[0]));
	}

	@Override
	public void removeFromGroup(long groupId, long[] userIds) throws ServerException {
		ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		Group group = groupDao.findById(groupId);
		for (long id : userIds) {
			User user = userDao.findById(id, true);
			user.removeGroup(group.getId());
			userDao.store(user);
		}
	}

	@Override
	public GUIGroup saveGroup(GUIGroup group) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
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
				groupDao.inheritACLs(grp.getId(), group.getInheritGroupId().longValue());
		}

		group.setId(grp.getId());
		return group;
	}

	@Override
	public GUIUser saveUser(GUIUser user, GUIInfo info) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		boolean createNew = false;
		String decodedPassword = "";

		try {
			// Disallow the editing of other users if you do not have access to
			// the Security
			if (user.getId() != session.getUserId() && getThreadLocalRequest() != null)
				ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);

			User usr;
			if (user.getId() != 0) {
				usr = userDao.findById(user.getId());
				userDao.initialize(usr);
			} else {
				usr = new User();
				createNew = true;
			}

			usr.setTenantId(session.getTenantId());
			usr.setCity(user.getCity());
			usr.setCountry(user.getCountry());
			usr.setEmail(user.getEmail());
			usr.setEmail2(user.getEmail2());
			usr.setFirstName(user.getFirstName());
			usr.setName(user.getName());
			usr.setLanguage(user.getLanguage());
			usr.setPostalcode(user.getPostalCode());
			usr.setState(user.getState());
			usr.setStreet(user.getAddress());
			usr.setTelephone(user.getPhone());
			usr.setTelephone2(user.getCell());
			usr.setUsername(user.getUserName());
			usr.setEnabled(user.isEnabled() ? 1 : 0);
			usr.setPasswordExpires(user.isPasswordExpires() ? 1 : 0);
			usr.setWelcomeScreen(user.getWelcomeScreen());
			usr.setIpWhiteList(user.getIpWhitelist());
			usr.setIpBlackList(user.getIpBlacklist());
			usr.setEmailSignature(user.getEmailSignature());
			usr.setDefaultWorkspace(user.getDefaultWorkspace());
			usr.setQuota(user.getQuota());
			usr.setSecondFactor(StringUtils.isEmpty(user.getSecondFactor()) ? null : user.getSecondFactor());
			usr.setKey(user.getKey());

			if (createNew) {
				User existingUser = userDao.findByUsername(user.getUserName());
				if (existingUser != null) {
					log.warn("Tried to create duplicate username " + user.getUserName());
					user.setWelcomeScreen(-99);
					return user;
				}

				// Generate an initial password
				ContextProperties pbean = Context.get().getProperties();
				int minsize = 8;
				try {
					minsize = pbean.getInt(session.getTenantName() + ".password.size");
				} catch (Throwable t) {

				}
				decodedPassword = new PasswordGenerator().generate(minsize);
				usr.setDecodedPassword(decodedPassword);

				if (user.isPasswordExpired()) {
					/*
					 * We want the user to change his password at the first
					 * login
					 */
					usr.setPasswordChanged(new Date(10000));
					usr.setPasswordExpired(1);
				} else
					usr.setPasswordChanged(new Date());
			}

			boolean stored = userDao.store(usr);
			if (!stored)
				throw new Exception("User not stored");
			user.setId(usr.getId());

			GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			usr.removeAllGroups();
			long[] ids = new long[user.getGroups().length];
			for (int i = 0; i < user.getGroups().length; i++) {
				ids[i] = user.getGroups()[i].getId();
				usr.addGroup(groupDao.findById(ids[i]));
			}
			userDao.store(usr);

			Group adminGroup = groupDao.findByName("admin", session.getTenantId());
			groupDao.initialize(adminGroup);

			// The admin user must be always member of admin group
			if ("admin".equals(user.getUserName()) && !user.isMemberOf("admin")) {
				usr.addGroup(adminGroup);
				userDao.store(usr);
			}

			// Notify the user by email
			if (createNew && user.isNotifyCredentials())
				try {
					notifyAccount(usr, decodedPassword);
				} catch (Throwable e) {
					log.warn(e.getMessage(), e);
				}

			return getUser(user.getId());
		} catch (Throwable t) {
			return (GUIUser) ServiceUtil.throwServerException(session, log, t);
		}
	}

	/**
	 * Notify the user with it's new account
	 * 
	 * @param user The created user
	 * @param password The decoded password
	 * @throws Exception
	 */
	private void notifyAccount(User user, String password) throws Exception {
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
		Map<String, Object> dictionary = new HashMap<String, Object>();
		ContextProperties config = Context.get().getProperties();
		String address = config.getProperty("server.url");
		dictionary.put("url", address);
		dictionary.put("user", user);
		dictionary.put("password", password);
		dictionary.put(Automation.LOCALE, locale);

		EMailSender sender = new EMailSender(user.getTenantId());
		sender.send(email, "psw.rec1", dictionary);
	}

	@Override
	public GUIUser saveProfile(GUIUser user) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

		try {
			// Disallow the editing of other users if you do not have access to
			// the Security
			if (user.getId() != session.getUserId())
				ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);

			User usr = userDao.findById(user.getId());
			userDao.initialize(usr);

			usr.setFirstName(user.getFirstName());
			usr.setName(user.getName());
			usr.setEmail(user.getEmail());
			usr.setEmail2(user.getEmail2());
			usr.setLanguage(user.getLanguage());
			usr.setStreet(user.getAddress());
			usr.setPostalcode(user.getPostalCode());
			usr.setCity(user.getCity());
			usr.setCountry(user.getCountry());
			usr.setState(user.getState());
			usr.setTelephone(user.getPhone());
			usr.setTelephone2(user.getCell());
			usr.setWelcomeScreen(user.getWelcomeScreen());
			usr.setDefaultWorkspace(user.getDefaultWorkspace());
			usr.setEmailSignature(user.getEmailSignature());
			usr.setEmailSignature2(user.getEmailSignature2());

			userDao.store(usr);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}

		return user;
	}

	@Override
	public void kill(String sid) {
		try {
			// Kill the LogicalDOC session
			SessionManager.get().kill(sid);

			SessionManager.get().removeSid(getThreadLocalRequest());

			// Also kill the servlet container session, if any
			HttpSession httpSession = SessionManager.get().getServletSession(sid);
			if (httpSession != null)
				httpSession.invalidate();
		} catch (Throwable t) {
			log.warn(t.getMessage());
		}
	}

	@Override
	public GUISecuritySettings loadSettings() throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		GUISecuritySettings securitySettings = new GUISecuritySettings();
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			ContextProperties pbean = Context.get().getProperties();

			securitySettings.setPwdExpiration(Integer.parseInt(pbean.getProperty("password.ttl")));
			securitySettings
					.setPwdSize(Integer.parseInt(pbean.getProperty(session.getTenantName() + ".password.size")));
			if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ".gui.savelogin")))
				securitySettings.setSaveLogin("true".equals(pbean.getProperty(session.getTenantName()
						+ ".gui.savelogin")));
			securitySettings.setIgnoreLoginCase("true".equals(pbean.getProperty("login.ignorecase")));
			if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ".anonymous.enabled")))
				securitySettings.setEnableAnonymousLogin("true".equals(pbean.getProperty(session.getTenantName()
						+ ".anonymous.enabled")));
			if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ".anonymous.key")))
				securitySettings.setAnonymousKey(pbean.getProperty(session.getTenantName() + ".anonymous.key"));
			if (StringUtils.isNotEmpty(pbean.getProperty(session.getTenantName() + ".anonymous.user"))) {
				User user = userDao.findByUsername(pbean.getProperty(session.getTenantName() + ".anonymous.user"));
				if (user != null)
					securitySettings.setAnonymousUser(getUser(user.getId()));
			}
			if (StringUtils.isNotEmpty(pbean.getProperty("ssl.required")))
				securitySettings.setForceSsl("true".equals(pbean.getProperty("ssl.required")));

			log.debug("Security settings data loaded successfully.");
		} catch (Exception e) {
			log.error("Exception loading Security settings data: " + e.getMessage(), e);
		}

		return securitySettings;
	}

	@Override
	public boolean saveSettings(GUISecuritySettings settings) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);

		boolean restartRequired = false;

		try {
			ContextProperties conf = Context.get().getProperties();

			if (session.getTenantId() == Tenant.DEFAULT_ID) {
				conf.setProperty("password.ttl", Integer.toString(settings.getPwdExpiration()));
				conf.setProperty("login.ignorecase", Boolean.toString(settings.isIgnoreLoginCase()));
				conf.setProperty("ssl.required", Boolean.toString(settings.isForceSsl()));

				// Update the web.xml
				try {
					ServletContext context = getServletContext();
					String policy = "true".equals(conf.getProperty("ssl.required")) ? "CONFIDENTIAL" : "NONE";
					WebConfigurator configurator = new WebConfigurator(context.getRealPath("/WEB-INF/web.xml"));
					restartRequired = configurator.setTransportGuarantee(policy);
				} catch (Throwable t) {
					log.error(t.getMessage());
				}
			}

			conf.setProperty(session.getTenantName() + ".password.size", Integer.toString(settings.getPwdSize()));
			conf.setProperty(session.getTenantName() + ".gui.savelogin", Boolean.toString(settings.isSaveLogin()));
			conf.setProperty(session.getTenantName() + ".anonymous.enabled",
					Boolean.toString(settings.isEnableAnonymousLogin()));
			conf.setProperty(session.getTenantName() + ".anonymous.key", settings.getAnonymousKey().trim());

			if (settings.getAnonymousUser() != null)
				conf.setProperty(session.getTenantName() + ".anonymous.user", settings.getAnonymousUser().getUserName());

			conf.write();

			log.info("Security settings data written successfully.");

			return restartRequired;
		} catch (Throwable e) {
			return (Boolean) ServiceUtil.throwServerException(session, log, e);
		}
	}

	private boolean saveRules(Session session, Menu menu, long userId, GUIRight[] rights) throws Exception {
		MenuDAO mdao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		if (!mdao.isReadEnable(Menu.SECURITY, session.getUserId()))
			throw new Exception("User " + session.getUsername() + " not allowed to save security settings");

		GroupDAO gdao = (GroupDAO) Context.get().getBean(GroupDAO.class);

		boolean sqlerrors = false;
		try {
			mdao.initialize(menu);
			menu.setSecurityRef(null);

			// Remove all current tenant rights
			Set<MenuGroup> grps = new HashSet<MenuGroup>();
			for (MenuGroup mg : menu.getMenuGroups()) {
				Group group = gdao.findById(mg.getGroupId());
				if (group != null && group.getTenantId() != session.getTenantId())
					grps.add(mg);
			}
			menu.getMenuGroups().clear();

			sqlerrors = false;
			for (GUIRight right : rights) {
				Group group = gdao.findById(right.getEntityId());
				if (group == null || group.getTenantId() != session.getTenantId())
					continue;

				MenuGroup fg = null;
				if (right.isRead()) {
					fg = new MenuGroup();
					fg.setGroupId(right.getEntityId());
				}
				grps.add(fg);
			}

			menu.setMenuGroups(grps);
			boolean stored = mdao.store(menu);
			if (!stored) {
				sqlerrors = true;
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
		return !sqlerrors;
	}

	@Override
	public void applyRights(GUIMenu menu) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SECURITY);
		try {
			MenuDAO mdao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			saveRules(session, mdao.findById(menu.getId()), session.getUserId(), menu.getRights());
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIMenu getMenu(long menuId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			GroupDAO gdao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			Menu menu = dao.findById(menuId);
			if (menu == null)
				return null;

			GUIMenu f = new GUIMenu();
			f.setId(menuId);

			int i = 0;
			GUIRight[] rights = new GUIRight[menu.getMenuGroups().size()];
			for (MenuGroup fg : menu.getMenuGroups()) {
				Group group = gdao.findById(fg.getGroupId());
				if (group == null || group.getTenantId() != session.getTenantId())
					continue;

				GUIRight right = new GUIRight();
				right.setEntityId(fg.getGroupId());
				rights[i] = right;
				i++;
			}
			f.setRights(rights);

			return f;
		} catch (Throwable e) {
			return (GUIMenu) ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public GUIUser[] searchUsers(String username, String groupId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);

			StringBuffer query = new StringBuffer(
					"select A.ld_id, A.ld_username, A.ld_name, A.ld_firstname from ld_user A ");
			if (StringUtils.isNotEmpty(groupId))
				query.append(", ld_usergroup B");
			query.append(" where A.ld_deleted=0 and A.ld_type=" + User.TYPE_DEFAULT);
			if (StringUtils.isNotEmpty(username))
				query.append(" and A.ld_username like '%" + SqlUtil.doubleQuotes(username) + "%'");
			if (StringUtils.isNotEmpty(groupId))
				query.append(" and A.ld_id=B.ld_userid and B.ld_groupid=" + Long.parseLong(groupId));

			@SuppressWarnings("unchecked")
			List<GUIUser> users = (List<GUIUser>) userDao.query(query.toString(), null, new RowMapper<GUIUser>() {

				@Override
				public GUIUser mapRow(ResultSet rs, int row) throws SQLException {
					GUIUser user = new GUIUser();
					user.setId(rs.getLong(1));
					user.setUserName(rs.getString(2));
					user.setName(rs.getString(3));
					user.setFirstName(rs.getString(4));
					return user;
				}
			}, null);

			return users.toArray(new GUIUser[0]);
		} catch (Throwable e) {
			return (GUIUser[]) ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public GUISequence[] loadBlockedEntities() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		if (session.getTenantId() != Tenant.DEFAULT_ID)
			return new GUISequence[0];

		try {
			ContextProperties config = Context.get().getProperties();
			SequenceDAO dao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			List<Sequence> seqs = new ArrayList<Sequence>();
			long max = config.getInt("throttle.username.max", 0);
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.MINUTE, -config.getInt("throttle.username.wait", 0));
			Date oldestDate = cal.getTime();
			if (max > 0)
				seqs.addAll(dao.findByWhere("_entity.name like '" + LoginThrottle.LOGINFAIL_USERNAME
						+ "%' and _entity.value >= ?1 and _entity.lastModified >= ?2",
						new Object[] { max, oldestDate }, null, null));

			max = config.getInt("throttle.ip.max", 0);
			cal = Calendar.getInstance();
			cal.add(Calendar.MINUTE, -config.getInt("throttle.ip.wait", 0));
			oldestDate = cal.getTime();
			if (max > 0)
				seqs.addAll(dao.findByWhere("_entity.name like '" + LoginThrottle.LOGINFAIL_IP
						+ "%' and _entity.value >= ?1 and _entity.lastModified >= ?2",
						new Object[] { max, oldestDate }, null, null));

			GUISequence[] ret = new GUISequence[seqs.size()];
			for (int i = 0; i < ret.length; i++) {
				ret[i] = new GUISequence();
				ret[i].setId(seqs.get(i).getId());
				ret[i].setValue(seqs.get(i).getValue());
				ret[i].setLastModified(seqs.get(i).getLastModified());
				ret[i].setName(seqs.get(i).getName());
			}

			return ret;
		} catch (Throwable e) {
			return (GUISequence[]) ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public void removeBlockedEntities(long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		if (session.getTenantId() != Tenant.DEFAULT_ID)
			return;

		try {
			SequenceDAO dao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			for (long id : ids) {
				dao.delete(id);
			}
		} catch (Throwable e) {
			ServiceUtil.throwServerException(session, log, e);
		}
	}
}