package com.logicaldoc.core.security.user;

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

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authentication.PasswordAlreadyUsedException;
import com.logicaldoc.core.security.authentication.PasswordWeakException;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.security.PasswordCriteria;
import com.logicaldoc.util.security.PasswordValidator;

import jakarta.annotation.Resource;

/**
 * Hibernate implementation of <code>UserDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateUserDAO extends HibernatePersistentObjectDAO<User> implements UserDAO {

	private static final String ADMIN = "admin";

	private static final String LOWER = "lower(";

	private static final String USERNAME = "username";

	@Resource(name = "GenericDAO")
	private GenericDAO genericDAO;

	@Resource(name = "UserHistoryDAO")
	private UserHistoryDAO userHistoryDAO;

	@Resource(name = "PasswordHistoryDAO")
	private PasswordHistoryDAO passwordHistoryDAO;

	@Resource(name = "userListenerManager")
	private UserListenerManager userListenerManager;

	@Resource(name = "ContextProperties")
	private ContextProperties config;

	private HibernateUserDAO() {
		super(User.class);
		super.log = LoggerFactory.getLogger(HibernateUserDAO.class);
	}

	public static boolean ignoreCaseLogin() {
		return "true".equals(Context.get().getProperties().getProperty("login.ignorecase"));
	}

	@Override
	public List<User> findByName(String name) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("name", name.toLowerCase());

		return findByWhere(LOWER + ENTITY + ".name) like :name", params, null, null);
	}

	@Override
	public User findByUsername(String username) throws PersistenceException {
		if (StringUtils.isEmpty(username))
			return null;

		User user = null;
		Map<String, Object> params = new HashMap<>();
		params.put(USERNAME, username);

		List<User> coll = findByWhere(ENTITY + ".username = :username", params, null, null);
		if (CollectionUtils.isNotEmpty(coll))
			user = coll.iterator().next();
		initialize(user);
		return user;
	}

	@Override
	public User findByUsernameIgnoreCase(String username) throws PersistenceException {
		if (StringUtils.isEmpty(username))
			return null;

		User user = null;
		Map<String, Object> params = new HashMap<>();
		params.put(USERNAME, username.toLowerCase());

		List<User> coll = findByWhere(LOWER + ENTITY + ".username) = :username", params, null, null);
		if (CollectionUtils.isNotEmpty(coll))
			user = coll.iterator().next();
		initialize(user);
		return user;
	}

	@Override
	public List<User> findByLikeUsername(String username) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put(USERNAME, username);
		return findByWhere(ENTITY + ".username like :username", params, null, null);
	}

	@Override
	public List<User> findByUsernameAndName(String username, String name) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put(USERNAME, username);
		params.put("name", name.toLowerCase());

		return findByWhere(LOWER + ENTITY + ".name) like :name and " + ENTITY + ".username like :username", params,
				null, null);
	}

	@Override
	public void checkPasswordCompliance(User user) throws PasswordWeakException, PersistenceException {

		// Skip the tests in case of new tenant creation
		if (user.getId() == 0L && user.getTenantId() != Tenant.DEFAULT_ID && "Administrator".equals(user.getFirstName())
				&& user.getUsername().startsWith(ADMIN) && ADMIN.equals(user.getDecodedPassword()))
			return;

		// Without decoded password we cannot perform any test
		String password = user.getDecodedPassword();
		if (StringUtils.isEmpty(password))
			return;

		TenantDAO tenantDAO = Context.get(TenantDAO.class);
		String tenant = tenantDAO.getTenantName(user.getTenantId());

		Map<String, String> messages = I18N.getMessages(user.getLocale() != null ? user.getLocale() : Locale.ENGLISH);
		List<String> errorKeys = messages.keySet().stream().filter(key -> key.startsWith("passwderror.")).toList();
		Properties props = new Properties();
		for (String key : errorKeys)
			props.put(key.substring(key.indexOf('.') + 1), messages.get(key));

		ContextProperties config = Context.get().getProperties();

		// length between X and 30 characters
		// at least X upper-case character
		// at least X lower-case characters
		// at least X digit characters
		// at least X digit characters
		PasswordCriteria criteria = new PasswordCriteria(config.getInt(tenant + ".password.size", 8),
				config.getInt(tenant + ".password.uppercase", 2), config.getInt(tenant + ".password.lowercase", 2),
				config.getInt(tenant + ".password.digit", 1), config.getInt(tenant + ".password.special", 1));

		// at least X times a character can be used
		criteria.setMaxOccurrences(config.getInt(tenant + ".password.occurrence", 3));

		// define some illegal sequences that will fail when >= 4 chars
		// long alphabetical is of the form 'abcd', numerical is '3456',
		// qwer is 'asdf' the false parameter indicates that wrapped
		// sequences are allowed; e.g. 'xyzab'
		criteria.setMaxSequenceSize(config.getInt(tenant + ".password.sequence", 3));

		PasswordValidator validator = new PasswordValidator(criteria, props);

		List<String> errors = validator.validate(password);
		if (!errors.isEmpty())
			throw new PasswordWeakException(errors);
	}

	private void checkAlreadyUsedPassword(User user) throws PersistenceException, PasswordAlreadyUsedException {
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

		validateUsernameUniquenes(user, newUser);

		if (user.getType() == UserType.SYSTEM)
			user.setType(UserType.DEFAULT);

		enforceReadOnlyUserGroups(user);

		Map<String, Object> dictionary = new HashMap<>();

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

		if (ADMIN.equals(user.getUsername()) && user.getPassword() == null)
			throw new PersistenceException(
					String.format("Trying to alter the %s user with null password", user.getUsername()));

		saveOrUpdate(user);
		if (newUser)
			flush();

		// Save the working times
		saveWorkingTimes(user);

		enforceUserGroupAssignment(user);

		/*
		 * Update the user-group assignments
		 */
		updateUserGroupAssignments(user);

		// Save the password history to track the password change
		recordPasswordChange(user, transaction, passwordChanged);

		invokeListenersAfter(user, transaction, dictionary);

		saveHistory(user, transaction, newUser);

		// If the admin password may have been changed and the 'adminpasswd'
		// also has to be updated
		updateAdminPasswordSetting(user);

		enforceReadOnlyUserPermissions(user);

		saveEnabledOrDisabledHistory(user, transaction, enabledStatusChanged);
	}

	private void validateUsernameUniquenes(User user, boolean newUser) throws PersistenceException {
		if (newUser && findByUsernameIgnoreCase(user.getUsername()) != null)
			throw new PersistenceException(String.format(
					"Another user exists with the same username %s (perhaps with different case)", user.getUsername()));
	}

	private void saveWorkingTimes(User user) throws PersistenceException {
		jdbcUpdate("delete from ld_workingtime where ld_userid=" + user.getId());
		if (user.getWorkingTimes() != null)
			for (WorkingTime wt : user.getWorkingTimes()) {
				Map<String, Object> params = new HashMap<>();
				params.put("userId", user.getId());
				params.put("dayOfWeek", wt.getDayOfWeek());
				params.put("hourStart", wt.getHourStart());
				params.put("minuteStart", wt.getMinuteStart());
				params.put("hourEnd", wt.getHourEnd());
				params.put("minuteEnd", wt.getMinuteEnd());
				params.put("label", wt.getLabel());
				params.put("description", wt.getDescription());

				jdbcUpdate(
						"insert into ld_workingtime(ld_userid,ld_dayofweek,ld_hourstart,ld_minutestart,ld_hourend,ld_minuteend,ld_label,ld_description) values (:userId, :dayOfWeek, :hourStart, :minuteStart, :hourEnd, :minuteEnd, :label, :description)",
						params);
			}
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
		GroupDAO groupDAO = Context.get(GroupDAO.class);
		String userGroupName = user.getUserGroupName();
		Group grp = groupDAO.findByName(userGroupName, user.getTenantId());
		if (grp == null) {
			grp = new Group();
			grp.setName(userGroupName);
			grp.setType(GroupType.USER);
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
	private void enforceReadOnlyUserGroups(User user) throws PersistenceException {
		if (user.isReadonly()) {
			GroupDAO gDao = Context.get(GroupDAO.class);
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
			GroupDAO groupDAO = Context.get(GroupDAO.class);
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
		if (ADMIN.equals(user.getUsername()) && user.getTenantId() == Tenant.DEFAULT_ID) {
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
			 * Save an history to gridRecord the user creation
			 */
			UserHistory createdHistory = new UserHistory();
			if (transaction != null)
				createdHistory = new UserHistory(transaction);
			createdHistory.setEvent(UserEvent.CREATED);
			createdHistory.setComment(user.getUsername());
			saveUserHistory(user, createdHistory);
		} else {
			if (transaction == null) {
				transaction = new UserHistory();
				transaction.setEvent(UserEvent.UPDATED);
				transaction.setComment(user.getUsername());
			}
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
		Map<String, Object> params = new HashMap<>();
		params.put("userId", user.getId());
		jdbcUpdate("delete from ld_usergroup where ld_userid = :userId", params);
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
	 * Saves an history to gridRecord the changes in the enabled status of a
	 * user
	 * 
	 * @param user The current user
	 * @param transaction the current session
	 * @param enabledStatusChanged A flag indicating if the status has been
	 *        modified
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private void saveEnabledOrDisabledHistory(User user, UserHistory transaction, boolean enabledStatusChanged)
			throws PersistenceException {
		if (enabledStatusChanged
				&& (transaction == null || (!transaction.getEvent().equals(UserEvent.DISABLED.toString())
						&& !transaction.getEvent().equals(UserEvent.ENABLED.toString())))) {
			UserHistory enabledOrDisabledHistory = new UserHistory();
			if (transaction != null)
				enabledOrDisabledHistory = new UserHistory(transaction);
			else {
				enabledOrDisabledHistory.setUser(user);
			}
			enabledOrDisabledHistory.setEvent(user.getEnabled() == 1 ? UserEvent.ENABLED : UserEvent.DISABLED);
			enabledOrDisabledHistory.setComment(null);
			saveUserHistory(user, enabledOrDisabledHistory);
		}
	}

	private void invokeListenersAfter(User user, UserHistory transaction, Map<String, Object> dictionary)
			throws AuthenticationException {
		log.debug("Invoke listeners after store");
		for (UserListener listener : userListenerManager.getListeners())
			try {
				listener.afterStore(user, transaction, dictionary);
			} catch (AuthenticationException ae) {
				throw ae;
			} catch (PersistenceException e) {
				log.warn("Error in listener {}", listener.getClass().getSimpleName(), e);
			}
	}

	private void invokeListenersBefore(User user, UserHistory transaction, Map<String, Object> dictionary)
			throws AuthenticationException {
		log.debug("Invoke listeners before store");
		for (UserListener listener : userListenerManager.getListeners())
			try {
				listener.beforeStore(user, transaction, dictionary);
			} catch (AuthenticationException ae) {
				throw ae;
			} catch (PersistenceException e) {
				log.warn("Error in listener {}", listener.getClass().getSimpleName(), e);
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
		// Do not implement password change logic in case of users imported from
		// another system
		if (user.getSource() != UserSource.DEFAULT)
			return false;

		boolean passwordChanged;
		String currentPassword = queryForString("select ld_password from ld_user where ld_id=" + user.getId());
		passwordChanged = currentPassword == null || !currentPassword.equals(user.getPassword());

		if (passwordChanged) {
			checkAlreadyUsedPassword(user);
			checkPasswordCompliance(user);
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

	private boolean isPasswordExpired(User user) throws PersistenceException {
		// Never consider changed the password of a user imported from another
		// system
		if (user == null || user.getSource() != UserSource.DEFAULT)
			return false;

		if (user.getPasswordExpired() == 1)
			return true;

		String tenantName = (Context.get(TenantDAO.class)).getTenantName(user.getTenantId());
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
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return true;
		}
	}

	private boolean isInactive(User user) throws PersistenceException {
		if (user == null)
			return false;

		if (user.getEnabled() == 0)
			return true;

		String tenantName = (Context.get(TenantDAO.class)).getTenantName(user.getTenantId());
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

		List<Date> interactions = queryForList(sb.toString(), Date.class);
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
	public boolean isInactive(String username) throws PersistenceException {
		return isInactive(findByUsernameIgnoreCase(username));
	}

	@Override
	public int count(Long tenantId) throws PersistenceException {
		String query = "select count(*) from ld_user where ld_type=" + UserType.DEFAULT.ordinal() + " and ld_deleted=0 "
				+ (tenantId != null ? " and ld_tenantid=" + tenantId : "");
		return queryForInt(query);
	}

	@Override
	public int countGuests(Long tenantId) throws PersistenceException {
		String query = "select count(*) from ld_user where ld_type=" + UserType.READONLY.ordinal()
				+ " and ld_deleted=0 " + (tenantId != null ? " and ld_tenantid=" + tenantId : "");
		return queryForInt(query);
	}

	@Override
	public void delete(long userId, int code) throws PersistenceException {
		delete(userId, null);
	}

	@Override
	public void delete(long userId, UserHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		User user = findById(userId);
		Group userGroup = user.getUserGroup();

		user.setDeleted(PersistentObject.DELETED_CODE_DEFAULT);
		user.setUsername(user.getUsername() + "." + user.getId());
		saveOrUpdate(user);

		// Delete the user's group
		if (userGroup != null) {
			GroupDAO groupDAO = Context.get(GroupDAO.class);
			groupDAO.delete(userGroup.getId());
		}

		jdbcUpdate("delete from ld_usergroup where ld_userid=" + userId);

		jdbcUpdate("delete from ld_apikey where ld_userid=" + userId);

		saveUserHistory(user, transaction);
	}

	private void saveUserHistory(User user, UserHistory transaction) throws PersistenceException {
		if (transaction == null)
			return;

		transaction.setUser(user);
		transaction.setNotified(0);
		userHistoryDAO.store(transaction);
	}

	private int getPasswordEnforce(User user) throws PersistenceException {
		TenantDAO tenantDAO = Context.get(TenantDAO.class);
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
	public void initialize(User user) throws PersistenceException {
		if (user == null || user.getId() == 0L)
			return;

		refresh(user);

		List<Long> groupIds = new ArrayList<>();
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
			GroupDAO gDao = Context.get(GroupDAO.class);
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

		// Manually initialize the collegtion of working times
		user.getWorkingTimes().clear();

		queryForResultSet(
				"select ld_dayofweek,ld_hourstart,ld_minutestart,ld_hourend,ld_minuteend,ld_label,ld_description from ld_workingtime where ld_userid="
						+ user.getId(),
				null, null, rows -> {
					while (rows.next()) {
						WorkingTime wt = new WorkingTime(rows.getInt(1), rows.getInt(2), rows.getInt(3));
						wt.setHourEnd(rows.getInt(4));
						wt.setMinuteEnd(rows.getInt(5));
						wt.setLabel(rows.getString(6));
						wt.setDescription(rows.getString(7));
						user.getWorkingTimes().add(wt);
					}
				});
	}

	@Override
	public Map<String, Generic> findUserSettings(long userId, String namePrefix) throws PersistenceException {
		List<Generic> generics = genericDAO.findByTypeAndSubtype("usersetting", namePrefix + "%", userId, null);
		Map<String, Generic> map = new HashMap<>();
		for (Generic generic : generics) {
			map.put(generic.getSubtype(), generic);
		}
		return map;
	}

	public void setGenericDAO(GenericDAO genericDAO) {
		this.genericDAO = genericDAO;
	}

	@Override
	public User findAdminUser(String tenantName) throws PersistenceException {
		if ("default".equals(tenantName))
			return findByUsername(ADMIN);
		else
			return findByUsername(ADMIN + StringUtils.capitalize(tenantName));
	}

	@Override
	public Set<User> findByGroup(long groupId) throws PersistenceException {
		List<Long> docIds = new ArrayList<>();
		try {
			docIds = queryForList("select ld_userid from ld_usergroup where ld_groupid=" + groupId, Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		Set<User> set = new HashSet<>();
		if (!docIds.isEmpty()) {
			String query = ENTITY + ".id in (" + StringUtil.arrayToString(docIds.toArray(new Long[0]), ",") + ")";
			List<User> users = findByWhere(query, (Map<String, Object>) null, null, null);
			for (User user : users) {
				if (user.getDeleted() == 0 && !set.contains(user))
					set.add(user);
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
	public User getUser(String username) throws PersistenceException {
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