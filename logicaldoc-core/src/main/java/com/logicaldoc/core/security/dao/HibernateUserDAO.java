package com.logicaldoc.core.security.dao;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.PasswordHistory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.UserGroup;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.UserListener;
import com.logicaldoc.core.security.UserListenerManager;
import com.logicaldoc.core.security.WorkingTime;
import com.logicaldoc.core.security.authentication.PasswordAlreadyUsedException;
import com.logicaldoc.core.security.authentication.PasswordWeakException;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.security.PasswordValidator;

/**
 * Hibernate implementation of <code>UserDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateUserDAO extends HibernatePersistentObjectDAO<User> implements UserDAO {

	private GenericDAO genericDAO;

	private UserHistoryDAO userHistoryDAO;

	private PasswordHistoryDAO passwordHistoryDAO;

	private UserListenerManager userListenerManager;

	private ContextProperties config;

	private HibernateUserDAO() {
		super(User.class);
		super.log = LoggerFactory.getLogger(HibernateUserDAO.class);
	}

	public static boolean ignoreCaseLogin() {
		return "true".equals(Context.get().getProperties().getProperty("login.ignorecase"));
	}

	@Override
	public List<User> findByName(String name) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("name", name.toLowerCase());

			return findByWhere("lower(" + ENTITY + ".name) like :name", params, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<User>();
		}
	}

	@Override
	public User findByUsername(String username) {
		User user = null;
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("username", username);

			List<User> coll = findByWhere(ENTITY + ".username = :username", params, null, null);
			if (coll.size() > 0)
				user = coll.iterator().next();
			initialize(user);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return user;
	}

	@Override
	public User findByUsernameIgnoreCase(String username) {
		User user = null;
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("username", username.toLowerCase());

			List<User> coll = findByWhere("lower(" + ENTITY + ".username) = :username", params, null, null);
			if (coll.size() > 0)
				user = coll.iterator().next();
			initialize(user);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return user;
	}

	/**
	 * @see com.logicaldoc.core.security.dao.UserDAO#findByUsername(java.lang.String)
	 */
	public List<User> findByLikeUsername(String username) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("username", username);

			return findByWhere(ENTITY + ".username like :username", params, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<User>();
		}
	}

	@Override
	public List<User> findByUsernameAndName(String username, String name) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("username", username);
			params.put("name", name.toLowerCase());

			return findByWhere("lower(" + ENTITY + ".name) like :name and " + ENTITY + ".username like :username",
					params, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<User>();
		}
	}

	private static void checkPasswordStrength(User user) throws PersistenceException {

		// Skip the tests in case of new tenant creation
		if (user.getId() == 0L && user.getTenantId() != Tenant.DEFAULT_ID && "Administrator".equals(user.getFirstName())
				&& user.getUsername().startsWith("admin") && "admin".equals(user.getDecodedPassword()))
			return;

		// Without decoded password we cannot perform any test
		String password = user.getDecodedPassword();
		if (StringUtils.isEmpty(password))
			return;

		TenantDAO tenantDAO = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenant = tenantDAO.getTenantName(user.getTenantId());

		Map<String, String> messages = I18N.getMessages(user.getLocale() != null ? user.getLocale() : Locale.ENGLISH);
		List<String> errorKeys = messages.keySet().stream().filter(key -> key.startsWith("passwderror."))
				.collect(Collectors.toList());
		Properties props = new Properties();
		for (String key : errorKeys)
			props.put(key.substring(key.indexOf('.') + 1), messages.get(key));

		ContextProperties config = Context.get().getProperties();

		PasswordValidator validator = new PasswordValidator(
				// length between X and 30 characters
				config.getInt(tenant + ".password.size", 8),

				// at least X upper-case character
				config.getInt(tenant + ".password.uppercase", 2),

				// at least X lower-case characters
				config.getInt(tenant + ".password.lowercase", 2),

				// at least X digit characters
				config.getInt(tenant + ".password.digit", 1),

				// at least X symbols (special character)
				config.getInt(tenant + ".password.special", 1),

				// define some illegal sequences that will fail when >= 4 chars
				// long alphabetical is of the form 'abcd', numerical is '3456',
				// qwer
				// is 'asdf' the false parameter indicates that wrapped
				// sequences are
				// allowed; e.g. 'xyzab'
				config.getInt(tenant + ".password.sequence", 3),

				// at least X times a character can be used
				config.getInt(tenant + ".password.occurrence", 3),

				props);

		List<String> errors = validator.validate(password);
		if (!errors.isEmpty())
			throw new PasswordWeakException(errors);
	}

	private void checkAlreadyUsedPassword(User user) throws PersistenceException {
		int enforce = getPasswordEnforce(user);
		if (enforce < 1)
			return;

		PasswordHistory hist = passwordHistoryDAO.findByUserIdAndPassword(user.getId(), user.getPassword(), enforce);
		if (hist != null)
			throw new PasswordAlreadyUsedException(hist.getDate());
	}

	@Override
	public void store(User user) throws PasswordAlreadyUsedException, PersistenceException {
		store(user, null);
	}

	@Override
	public void store(User user, UserHistory transaction) throws PasswordAlreadyUsedException, PersistenceException {
		if (!checkStoringAspect())
			return;

		boolean newUser = user.getId() == 0;
		boolean passwordChanged = false;
		boolean enabledStatusChanged = false;

		passwordChanged = processPasswordChanged(user);

		if (newUser && findByUsernameIgnoreCase(user.getUsername()) != null)
			throw new PersistenceException(String.format(
					"Another user exists with the same username %s (perhaps with different case)", user.getUsername()));

		if (user.getType() == User.TYPE_SYSTEM)
			user.setType(User.TYPE_DEFAULT);

		enforceReadOnlyUserGroups(user);

		Map<String, Object> dictionary = new HashMap<String, Object>();

		invokeListenersBefore(user, transaction, dictionary);

		if (newUser) {
			user.setCreation(new Date());
			user.setLastEnabled(user.getCreation());
		} else {
			int currentEnabled = queryForInt("select ld_enabled from ld_user where ld_id=" + user.getId());
			enabledStatusChanged = currentEnabled != user.getEnabled();

			// Record the last enabled date in case the user is getting
			// enabled
			if (enabledStatusChanged && user.getEnabled() == 1)
				user.setLastEnabled(new Date());

			// In any case the last enabled must at least equal the creation
			// date
			if (user.getLastEnabled() == null)
				user.setLastEnabled(user.getCreation());

			user.setPasswordExpired(isPasswordExpired(user) ? 1 : 0);
		}

		if ("admin".equals(user.getUsername()) && user.getPassword() == null)
			throw new PersistenceException(
					String.format("Trying to alter the %s user with null password", user.getUsername()));

		saveOrUpdate(user);

		enforceUserGroupAssignment(user);

		/*
		 * Update the user-group assignments
		 */
		updateUserGroupAssignments(user);

		// Save the password history to track the password change
		recordPasswordChange(user, transaction, passwordChanged);

		infokeListenersAfter(user, transaction, dictionary);

		saveHistory(user, transaction, newUser);

		// If the admin password may have been changed and the 'adminpasswd'
		// also has to
		// be updated
		updateAdminPasswordSetting(user);

		enforceReadOnlyUserPermissions(user);

		saveEnabledOrDisabledHistory(user, transaction, enabledStatusChanged);
	}

	/**
	 * Enforces the assignment of the special user group(the group that also
	 * represents the user itself)
	 * 
	 * @param user The current user
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void enforceUserGroupAssignment(User user) throws PersistenceException {
		GroupDAO groupDAO = (GroupDAO) Context.get().getBean(GroupDAO.class);
		String userGroupName = user.getUserGroupName();
		Group grp = groupDAO.findByName(userGroupName, user.getTenantId());
		if (grp == null) {
			grp = new Group();
			grp.setName(userGroupName);
			grp.setType(Group.TYPE_USER);
			grp.setTenantId(user.getTenantId());
			groupDAO.store(grp);
		}
		if (!user.isMemberOf(grp.getName()))
			user.addGroup(grp);
	}

	/**
	 * Saves the password history to track the password change
	 * 
	 * @param user The current user
	 * @param transaction The current session
	 * @param passwordChanged A flag indicating
	 * 
	 * @throws PersistenceException
	 */
	private void recordPasswordChange(User user, UserHistory transaction, boolean passwordChanged)
			throws PersistenceException {
		if (passwordChanged) {
			PasswordHistory pHist = new PasswordHistory();
			pHist.setDate(transaction != null ? transaction.getDate() : new Date());
			pHist.setUserId(user.getId());
			pHist.setPassword(user.getPassword());
			pHist.setTenantId(user.getTenantId());
			passwordHistoryDAO.store(pHist);
			passwordHistoryDAO.cleanOldHistories(user.getId(), getPasswordEnforce(user) * 2);
		}
	}

	/**
	 * Manipulates a read-only user in order to enforce the right group
	 * assignments
	 * 
	 * @param user The current user
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void enforceReadOnlyUserGroups(User user) {
		if (user.isReadonly()) {
			GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			Group guestGroup = gDao.findByName("guest", user.getTenantId());
			Group userGroup = user.getUserGroup();
			user.removeGroupMemberships(null);
			user.addGroup(userGroup);
			user.addGroup(guestGroup);
		}
	}

	/**
	 * Manipulates a read-only user in order to enforce the right guest
	 * permissions
	 * 
	 * @param user The current user
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void enforceReadOnlyUserPermissions(User user) throws PersistenceException {
		if (user.isReadonly()) {
			user = findById(user.getId());
			initialize(user);
			GroupDAO groupDAO = (GroupDAO) Context.get().getBean(GroupDAO.class);
			for (Group group : user.getGroups())
				groupDAO.fixGuestPermissions(group);
		}
	}

	/**
	 * Updates the 'adminpasswd' setting if the user is the administrator
	 * 
	 * @param user the current user
	 */
	private void updateAdminPasswordSetting(User user) {
		if ("admin".equals(user.getUsername()) && user.getTenantId() == Tenant.DEFAULT_ID) {
			log.info("Updated adminpasswd");
			config.setProperty("adminpasswd", user.getPassword());
			try {
				config.write();
			} catch (IOException e) {
				log.warn("Cannot write the new admin password in the configuration file", e);
			}
		}
	}

	/**
	 * Prepares and saves the right history when a user gets stored
	 * 
	 * @param user The current user
	 * @param transaction The current transaction
	 * @param newUser A flag indicating if the user is newly created
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void saveHistory(User user, UserHistory transaction, boolean newUser) throws PersistenceException {
		if (newUser) {
			saveDefaultDashlets(user);

			/*
			 * Save an history to record the user creation
			 */
			UserHistory createdHistory = new UserHistory();
			if (transaction != null)
				createdHistory = new UserHistory(transaction);
			createdHistory.setEvent(UserEvent.CREATED.toString());
			createdHistory.setComment(user.getUsername());
			saveUserHistory(user, createdHistory);
		} else {
			saveUserHistory(user, transaction);
		}
	}

	/**
	 * Update the user-group assignments
	 * 
	 * @param user the current user
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void updateUserGroupAssignments(User user) throws PersistenceException {
		jdbcUpdate("delete from ld_usergroup where ld_userid = ?", user.getId());
		for (UserGroup ug : user.getUserGroups()) {
			int exists = queryForInt("select count(*) from ld_group where ld_id=" + ug.getGroupId());
			if (exists > 0) {
				jdbcUpdate("insert into ld_usergroup(ld_userid, ld_groupid) values (" + user.getId() + ", "
						+ ug.getGroupId() + ")");
			} else
				log.warn("It seems that the usergroup {} does not exist anymore", ug.getGroupId());
		}
	}

	/**
	 * Saves an history to record the changes in the enabled status of a user
	 * 
	 * @param user The current user
	 * @param transaction the current session
	 * @param enabledStatusChanged A flag indicating if the status has been
	 *        modified
	 */
	private void saveEnabledOrDisabledHistory(User user, UserHistory transaction, boolean enabledStatusChanged) {
		if (enabledStatusChanged
				&& (transaction == null || (!transaction.getEvent().equals(UserEvent.DISABLED.toString())
						&& !transaction.getEvent().equals(UserEvent.ENABLED.toString())))) {
			UserHistory enabledOrDisabledHistory = new UserHistory();
			if (transaction != null)
				enabledOrDisabledHistory = new UserHistory(transaction);
			else {
				enabledOrDisabledHistory.setUser(user);
			}
			enabledOrDisabledHistory
					.setEvent(user.getEnabled() == 1 ? UserEvent.ENABLED.toString() : UserEvent.DISABLED.toString());
			enabledOrDisabledHistory.setComment(null);
			saveUserHistory(user, enabledOrDisabledHistory);
		}
	}

	private void infokeListenersAfter(User user, UserHistory transaction, Map<String, Object> dictionary) {
		log.debug("Invoke listeners after store");
		for (UserListener listener : userListenerManager.getListeners())
			try {
				listener.afterStore(user, transaction, dictionary);
			} catch (Exception e1) {
				log.warn("Error in listener {}", listener.getClass().getSimpleName(), e1);
			}
	}

	private void invokeListenersBefore(User user, UserHistory transaction, Map<String, Object> dictionary) {
		log.debug("Invoke listeners before store");
		for (UserListener listener : userListenerManager.getListeners())
			try {
				listener.beforeStore(user, transaction, dictionary);
			} catch (Exception e1) {
				log.warn("Error in listener {}", listener.getClass().getSimpleName(), e1);
			}
	}

	/**
	 * Detects ad handles the password change. It verifies that the new password
	 * has not been already used and complies with the quality rules.
	 * 
	 * @param user The current user
	 * 
	 * @return true id the password has been changed
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private boolean processPasswordChanged(User user) throws PersistenceException {
		boolean passwordChanged;
		String currentPassword = queryForString("select ld_password from ld_user where ld_id=" + user.getId());
		passwordChanged = currentPassword == null || !currentPassword.equals(user.getPassword());

		if (passwordChanged) {
			checkAlreadyUsedPassword(user);
			checkPasswordStrength(user);
		}
		return passwordChanged;
	}

	private void saveDefaultDashlets(User user) throws PersistenceException {
		String type = "usersetting";
		String dashletSubtype = "dashlet-";
		String[] dashletSubtypes = new String[] { dashletSubtype + "checkout", dashletSubtype + "locked",
				dashletSubtype + "notes" };
		for (int i = 0; i < dashletSubtypes.length; i++) {
			Generic dash = new Generic(type, dashletSubtypes[i], user.getId());
			dash.setString1("0");
			dash.setTenantId(user.getTenantId());
			switch (i) {
			case 0:
				dash.setInteger1(1L);
				dash.setInteger2(0L);
				dash.setInteger3(0L);
				break;
			case 1:
				dash.setInteger1(3L);
				dash.setInteger2(0L);
				dash.setInteger3(1L);
				break;
			case 3:
				dash.setInteger1(6L);
				dash.setInteger2(1L);
				dash.setInteger3(0L);
				break;
			default:
				// do nothing
			}
			genericDAO.store(dash);
		}
	}

	/**
	 * @see com.logicaldoc.core.security.dao.UserDAO#validateUser(java.lang.String,
	 *      java.lang.String)
	 */
	public boolean validateUser(String username, String password) {
		boolean result = true;

		try {
			User user = null;
			if (ignoreCaseLogin())
				user = findByUsernameIgnoreCase(username);
			else
				user = findByUsername(username);

			if (!validateUser(user))
				return false;

			// Check the password match
			if (user.getPassword() == null || !user.getPassword().equals(CryptUtil.cryptString(password)))
				result = false;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public boolean validateUser(String username) {
		boolean result = true;
		try {
			if (ignoreCaseLogin())
				result = validateUser(findByUsername(username));
			else
				result = validateUser(findByUsernameIgnoreCase(username));
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	private boolean validateUser(User user) {
		// Check the password match
		if (user == null || (user.getType() != User.TYPE_DEFAULT && user.getType() != User.TYPE_READONLY))
			return false;

		// Check if the user is enabled
		if (user != null && user.getEnabled() == 0)
			return false;

		if (isPasswordExpired(user.getUsername()))
			return false;

		return true;
	}

	private boolean isPasswordExpired(User user) {
		if (user == null)
			return false;

		if (user.getPasswordExpired() == 1)
			return true;

		String tenantName = ((TenantDAO) Context.get().getBean(TenantDAO.class)).getTenantName(user.getTenantId());
		int passwordTtl = config.getInt(tenantName + ".password.ttl", 90);
		if (passwordTtl <= 0)
			return false;

		// Check if the password is expired
		if (user.getPasswordExpires() == 1) {
			Date lastChange = user.getPasswordChanged();
			if (lastChange == null)
				return false;
			Calendar calendar = new GregorianCalendar();
			calendar.setTime(lastChange);
			calendar.set(Calendar.MILLISECOND, 0);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MINUTE, 0);
			calendar.set(Calendar.HOUR, 0);
			lastChange = calendar.getTime();

			calendar.setTime(new Date());
			calendar.set(Calendar.MILLISECOND, 0);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MINUTE, 0);
			calendar.set(Calendar.HOUR, 0);

			calendar.add(Calendar.DAY_OF_MONTH, -passwordTtl);
			Date date = calendar.getTime();

			return (lastChange.before(date));
		} else
			return false;
	}

	@Override
	public boolean isPasswordExpired(String username) {
		try {
			User user = findByUsernameIgnoreCase(username);
			return isPasswordExpired(user);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return true;
		}
	}

	private boolean isInactive(User user) throws PersistenceException {
		if (user == null)
			return false;

		if (user.getEnabled() == 0)
			return true;

		String tenantName = ((TenantDAO) Context.get().getBean(TenantDAO.class)).getTenantName(user.getTenantId());
		int maxInactiveDays = config.getInt(tenantName + ".security.user.maxinactivity", -1);
		if (user.getMaxInactivity() != null)
			maxInactiveDays = user.getMaxInactivity();
		if (maxInactiveDays <= 0)
			return false;

		log.info("Checking if the user {} has interactions in the last {} days", user.getUsername(), maxInactiveDays);

		StringBuilder sb = new StringBuilder(
				"select max(ld_date) from ld_history where ld_deleted=0 and ld_userid=" + user.getId());
		sb.append(
				" UNION select max(ld_date) from ld_user_history where ld_deleted=0 and not ld_event in ('event.user.updated', 'event.user.disabled', 'event.user.timeout', 'event.user.login.failed', 'event.user.deleted', 'event.user.messagereceived') and ld_userid="
						+ user.getId());
		sb.append(" UNION select max(ld_date) from ld_folder_history where ld_deleted=0 and ld_userid=" + user.getId()
				+ " order by 1 desc");
		List<Date> interactions = (List<Date>) queryForList(sb.toString(), Date.class);
		Date lastInteraction = null;
		if (!interactions.isEmpty())
			lastInteraction = interactions.get(0);

		// Perhaps the user never had interactions until now so we use his
		// creation date as last interaction
		if (lastInteraction == null)
			lastInteraction = user.getCreation();

		// In case the user has been enabled again after being disabled for
		// inactivity we should consider the last enabled date as last
		// interaction
		if (user.getLastEnabled() != null && user.getLastEnabled().after(lastInteraction))
			lastInteraction = user.getLastEnabled();

		Calendar calendar = new GregorianCalendar();
		calendar.setTime(lastInteraction);
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR, 0);
		lastInteraction = calendar.getTime();

		calendar.setTime(new Date());
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR, 0);

		calendar.add(Calendar.DAY_OF_MONTH, -maxInactiveDays);
		Date date = calendar.getTime();

		return (lastInteraction.before(date));
	}

	@Override
	public boolean isInactive(String username) {
		try {
			User user = findByUsernameIgnoreCase(username);
			return isInactive(user);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return true;
		}
	}

	@Override
	public int count(Long tenantId) {
		String query = "select count(*) from ld_user where ld_type=" + User.TYPE_DEFAULT + " and not(ld_deleted=1) "
				+ (tenantId != null ? " and ld_tenantid=" + tenantId : "");

		try {
			return queryForInt(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public int countGuests(Long tenantId) {
		String query = "select count(*) from ld_user where ld_type=" + User.TYPE_READONLY + " and not(ld_deleted=1) "
				+ (tenantId != null ? " and ld_tenantid=" + tenantId : "");

		try {
			return queryForInt(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public void delete(long userId, int code) throws PersistenceException {
		delete(userId, null);
	}

	@Override
	public void delete(long userId, UserHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		User user = (User) findById(userId);
		Group userGroup = user.getUserGroup();

		if (user != null) {
			user.setDeleted(PersistentObject.DELETED_CODE_DEFAULT);
			user.setUsername(user.getUsername() + "." + user.getId());
			saveOrUpdate(user);
		}

		// Delete the user's group
		if (userGroup != null) {
			GroupDAO groupDAO = (GroupDAO) Context.get().getBean(GroupDAO.class);
			groupDAO.delete(userGroup.getId());
		}

		jdbcUpdate("delete from ld_usergroup where ld_userid=" + userId);

		saveUserHistory(user, transaction);
	}

	private void saveUserHistory(User user, UserHistory transaction) {
		if (transaction == null)
			return;

		transaction.setUser(user);
		transaction.setNotified(0);

		try {
			userHistoryDAO.store(transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	private int getPasswordEnforce(User user) {
		TenantDAO tenantDAO = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String tenant = tenantDAO.getTenantName(user.getTenantId());
		return config.getInt(tenant + ".password.enforcehistory", 10);
	}

	public UserHistoryDAO getUserHistoryDAO() {
		return userHistoryDAO;
	}

	public void setUserHistoryDAO(UserHistoryDAO userHistoryDAO) {
		this.userHistoryDAO = userHistoryDAO;
	}

	public void setUserListenerManager(UserListenerManager userListenerManager) {
		this.userListenerManager = userListenerManager;
	}

	public UserListenerManager getUserListenerManager() {
		return userListenerManager;
	}

	@Override
	public void initialize(User user) {
		if (user == null)
			return;

		refresh(user);

		for (WorkingTime wt : user.getWorkingTimes())
			log.debug("Initializing working time {}", wt.getLabel());

		List<Long> groupIds = new ArrayList<Long>();
		try {
			groupIds = queryForList("select distinct ld_groupid from ld_usergroup where ld_userid=" + user.getId(),
					Long.class);
			for (Long groupId : groupIds)
				user.getUserGroups().add(new UserGroup(groupId));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		user.getGroups().clear();
		user.getUserGroups().clear();
		if (!groupIds.isEmpty()) {
			GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			try {
				List<Group> groups = gDao.findByWhere(
						ENTITY + ".id in (" + StringUtil.arrayToString(groupIds.toArray(new Long[0]), ",") + ")", null,
						null);
				for (Group group : groups) {
					user.getGroups().add(group);
					user.getUserGroups().add(new UserGroup(group.getId()));
				}
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	@Override
	public Map<String, Generic> findUserSettings(long userId, String namePrefix) {
		List<Generic> generics = genericDAO.findByTypeAndSubtype("usersetting", namePrefix + "%", userId, null);
		Map<String, Generic> map = new HashMap<String, Generic>();
		for (Generic generic : generics) {
			map.put(generic.getSubtype(), generic);
		}
		return map;
	}

	public void setGenericDAO(GenericDAO genericDAO) {
		this.genericDAO = genericDAO;
	}

	@Override
	public User findAdminUser(String tenantName) {
		if ("default".equals(tenantName))
			return findByUsername("admin");
		else
			return findByUsername("admin" + StringUtils.capitalize(tenantName));
	}

	@Override
	public Set<User> findByGroup(long groupId) {
		List<Long> docIds = new ArrayList<Long>();
		try {
			docIds = queryForList("select ld_userid from ld_usergroup where ld_groupid=" + groupId, Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		Set<User> set = new HashSet<User>();
		if (!docIds.isEmpty()) {
			String query = ENTITY + ".id in (" + StringUtil.arrayToString(docIds.toArray(new Long[0]), ",") + ")";

			try {
				List<User> users = findByWhere(query, (Map<String, Object>) null, null, null);
				for (User user : users) {
					if (user.getDeleted() == 0 && !set.contains(user))
						set.add(user);
				}
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return set;
	}

	@Override
	public User findById(long id) throws PersistenceException {
		User user = super.findById(id);
		if (user != null)
			initialize(user);
		return user;
	}

	@Override
	public User getUser(String username) {
		User user = null;
		if (HibernateUserDAO.ignoreCaseLogin())
			user = findByUsernameIgnoreCase(username);
		else
			user = findByUsername(username);
		initialize(user);
		return user;
	}

	public void setPasswordHistoryDAO(PasswordHistoryDAO passwordHistoryDAO) {
		this.passwordHistoryDAO = passwordHistoryDAO;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}
}