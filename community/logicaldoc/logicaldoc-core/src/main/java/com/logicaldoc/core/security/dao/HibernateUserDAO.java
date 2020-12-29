package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.UserGroup;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.UserListener;
import com.logicaldoc.core.security.UserListenerManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.crypt.CryptUtil;

/**
 * Hibernate implementation of <code>UserDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateUserDAO extends HibernatePersistentObjectDAO<User> implements UserDAO {

	private GenericDAO genericDAO;

	private UserHistoryDAO userHistoryDAO;

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
	public boolean delete(long userId, int code) {
		return delete(userId, null);
	}

	@Override
	public List<User> findByName(String name) {
		try {
			return findByWhere("lower(_entity.name) like ?1", new Object[] { name.toLowerCase() }, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<User>();
		}
	}

	@Override
	public User findByUsername(String username) {
		User user = null;
		try {
			List<User> coll = findByWhere("_entity.username = ?1", new Object[] { username }, null, null);
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
			List<User> coll = findByWhere("lower(_entity.username) = ?1", new Object[] { username.toLowerCase() }, null,
					null);
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
			return findByWhere("_entity.username like ?1", new Object[] { username }, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<User>();
		}
	}

	@Override
	public List<User> findByUsernameAndName(String username, String name) {
		try {
			return findByWhere("lower(_entity.name) like ?1 and _entity.username like ?2",
					new Object[] { name.toLowerCase(), username }, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<User>();
		}
	}

	@Override
	public boolean store(User user) {
		return store(user, null);
	}

	@Override
	public boolean store(User user, UserHistory transaction) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;
		boolean newUser = user.getId() == 0;

		try {
			if ("admin".equals(user.getUsername()) && user.getType() != User.TYPE_DEFAULT)
				throw new Exception("User admin must be default type");
			if (newUser && findByUsernameIgnoreCase(user.getUsername()) != null)
				throw new Exception(
						String.format("Another user exists with the same username %s (perhaps with different case)",
								user.getUsername()));

			if (user.getType() == User.TYPE_SYSTEM)
				user.setType(User.TYPE_DEFAULT);

			if (user.isReadonly()) {
				GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
				Group guestGroup = gDao.findByName("guest", user.getTenantId());
				Group userGroup = user.getUserGroup();
				user.removeGroupMemberships(null);
				user.addGroup(userGroup);
				user.addGroup(guestGroup);
			}

			Map<String, Object> dictionary = new HashMap<String, Object>();

			log.debug("Invoke listeners before store");
			for (UserListener listener : userListenerManager.getListeners())
				try {
					listener.beforeStore(user, transaction, dictionary);
				} catch (Throwable t) {
					log.warn(t.getMessage(), t);
				}

			if (newUser)
				user.setCreation(new Date());

			saveOrUpdate(user);

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

			/*
			 * Update the user-group assignments
			 */
			{
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

			log.debug("Invoke listeners after store");
			for (UserListener listener : userListenerManager.getListeners())
				try {
					listener.afterStore(user, transaction, dictionary);
				} catch (Throwable t) {
					log.warn(t.getMessage(), t);
				}

			if (newUser) {
				// Save default dashlets
				Generic dash = new Generic("usersetting", "dashlet-checkout", user.getId());
				dash.setInteger1(1L);
				dash.setInteger2(0L);
				dash.setInteger3(0L);
				dash.setString1("0");
				dash.setTenantId(user.getTenantId());
				genericDAO.store(dash);
				dash = new Generic("usersetting", "dashlet-locked", user.getId());
				dash.setInteger1(3L);
				dash.setInteger2(0L);
				dash.setInteger3(1L);
				dash.setString1("0");
				dash.setTenantId(user.getTenantId());
				genericDAO.store(dash);
				dash = new Generic("usersetting", "dashlet-notes", user.getId());
				dash.setInteger1(6L);
				dash.setInteger2(1L);
				dash.setInteger3(0L);
				dash.setString1("0");
				dash.setTenantId(user.getTenantId());
				genericDAO.store(dash);

				/*
				 * Save an history to record the user creation
				 */
				UserHistory createdHistory = new UserHistory();
				if (transaction != null)
					createdHistory = (UserHistory) transaction.clone();
				createdHistory.setEvent(UserEvent.CREATED.toString());
				createdHistory.setComment(user.getUsername());
				saveUserHistory(user, createdHistory);
			} else if (transaction != null)
				saveUserHistory(user, transaction);

			// If the admin password was changed the 'adminpasswd' also has to
			// be updated
			if ("admin".equals(user.getUsername()) && user.getTenantId() == Tenant.DEFAULT_ID) {
				log.info("Updated adminpasswd");
				config.setProperty("adminpasswd", user.getPassword());
				config.write();
			}

			if (user.isReadonly()) {
				for (Group group : user.getGroups())
					groupDAO.fixGuestPermissions(group);
			}
		} catch (Throwable e) {
			if (e instanceof AuthenticationException)
				throw (AuthenticationException) e;
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
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
			if (!user.getPassword().equals(CryptUtil.cryptString(password)))
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

	public int getPasswordTtl() {
		int value = 90;
		if (config.getProperty("password.ttl") != null)
			value = config.getInt("password.ttl");
		return value;
	}

	@Override
	public boolean isPasswordExpired(String username) {
		try {
			User user = findByUsernameIgnoreCase(username);
			if (user == null)
				return false;

			if (user.getPasswordExpired() == 1)
				return true;

			if (getPasswordTtl() <= 0)
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

				calendar.add(Calendar.DAY_OF_MONTH, -getPasswordTtl());
				Date date = calendar.getTime();

				return (lastChange.before(date));
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return true;
		}
		return false;
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
	public boolean delete(long userId, UserHistory transaction) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;

		try {
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
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
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
						"_entity.id in (" + StringUtil.arrayToString(groupIds.toArray(new Long[0]), ",") + ")", null,
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

	public void setConfig(ContextProperties config) {
		this.config = config;
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
			String query = "_entity.id in (" + StringUtil.arrayToString(docIds.toArray(new Long[0]), ",") + ")";

			try {
				List<User> users = findByWhere(query, null, null, null);
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
	public User findById(long id) {
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
}