package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.WorkingTime;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.model.WSGroup;
import com.logicaldoc.webservice.model.WSUser;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.model.WSWorkingTime;
import com.logicaldoc.webservice.soap.SecurityService;

/**
 * Security Web Service Implementation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapSecurityService extends AbstractService implements SecurityService {
	protected static Logger log = LoggerFactory.getLogger(SoapSecurityService.class);

	@Override
	public WSUser[] listUsers(String sid, String group) throws Exception {
		User user = validateSession(sid);

		try {
			List<WSUser> users = new ArrayList<WSUser>();
			UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
			if (StringUtils.isEmpty(group)) {
				for (User usr : dao.findAll(user.getTenantId())) {
					dao.initialize(user);
					if (usr.getType() != User.TYPE_SYSTEM)
						users.add(WSUser.fromUser(usr));
				}
			} else {
				GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
				Group grp = gDao.findByName(group, user.getTenantId());
				gDao.initialize(grp);
				for (User usr : grp.getUsers()) {
					dao.initialize(user);
					if (usr.getType() != User.TYPE_SYSTEM)
						users.add(WSUser.fromUser(usr));
				}
			}

			// Remove sensible informations in case of non admin user
			if (!user.isMemberOf(Group.GROUP_ADMIN)) {
				for (WSUser usr : users) {
					usr.setUsername(null);
					usr.setEmail(null);
					usr.setEmail2(null);
					usr.setPassword(null);
					usr.setPasswordmd4(null);
					usr.setPostalcode(null);
					usr.setStreet(null);
					usr.setTelephone(null);
					usr.setTelephone2(null);
				}
			}

			return users.toArray(new WSUser[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public WSGroup[] listGroups(String sid) throws Exception {
		checkAdministrator(sid);
		User user = validateSession(sid);

		try {
			List<WSGroup> groups = new ArrayList<WSGroup>();
			GroupDAO dao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			for (Group grp : dao.findAll(user.getTenantId())) {
				if (grp.getType() == Group.TYPE_DEFAULT) {
					dao.initialize(grp);
					groups.add(WSGroup.fromGroup(grp));
				}
			}
			return groups.toArray(new WSGroup[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public long storeUser(String sid, WSUser user) throws Exception {
		checkAdministrator(sid);
		User sessionUser = validateSession(sid);

		try {
			GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
			User usr = user.toUser();
			usr.setTenantId(sessionUser.getTenantId());

			if (user.getId() != 0) {
				usr = dao.findById(user.getId());
				if (usr.getType() == User.TYPE_SYSTEM)
					throw new Exception("You cannot edit user with id " + usr.getId() + " because it is a system user");
				dao.initialize(usr);

				usr.setCity(user.getCity());
				usr.setCountry(user.getCountry());
				usr.setEmail(user.getEmail());
				usr.setEmailSignature(user.getEmailSignature());
				usr.setEmail2(user.getEmail2());
				usr.setEmailSignature2(user.getEmailSignature2());
				usr.setFirstName(user.getFirstName());
				usr.setName(user.getName());
				usr.setLanguage(user.getLanguage());
				usr.setPostalcode(user.getPostalcode());
				usr.setState(user.getState());
				usr.setStreet(user.getStreet());
				usr.setTelephone(user.getTelephone());
				usr.setTelephone2(user.getTelephone2());
				usr.setUsername(user.getUsername());
				usr.setEnabled(user.getEnabled());
				usr.setPasswordExpires(user.getPasswordExpires());
				usr.setQuota(user.getQuota());
				usr.setType(user.getType());
				usr.setSource(user.getSource());
				usr.setDateFormat(user.getDateFormat());
				usr.setDateFormatShort(user.getDateFormatShort());
				usr.setDateFormatLong(user.getDateFormatLong());
				usr.setKey(user.getKey());
				usr.setSecondFactor(user.getSecondFactor());
				usr.setTimeZone(user.getTimeZone());
				usr.setExpire(WSUtil.convertStringToDate(user.getExpire()));
				usr.setEnforceWorkingTime(user.getEnforceWorkingTime());
				usr.setMaxInactivity(user.getMaxInactivity());

				if (user.getWorkingTimes() != null && user.getWorkingTimes().length > 0)
					for (WSWorkingTime wswt : user.getWorkingTimes()) {
						WorkingTime wt = new WorkingTime();
						BeanUtils.copyProperties(wt, wswt);
						usr.getWorkingTimes().add(wt);
					}
			} else {
				usr.setDecodedPassword(user.getDecodedPassword());
			}

			if (StringUtils.isEmpty(usr.getUsername()))
				throw new Exception("Missing mandatory value 'UserName'");
			else if (StringUtils.isEmpty(usr.getEmail()))
				throw new Exception("Missing mandatory value 'Email'");
			else if (StringUtils.isEmpty(usr.getName()))
				throw new Exception("Missing mandatory value 'Name'");
			else if (StringUtils.isEmpty(usr.getFirstName()))
				throw new Exception("Missing mandatory value 'FirstName'");

			if (!dao.store(usr))
				throw new Exception("Unable to store the user");

			if (user.getGroupIds() != null && user.getGroupIds().length > 0) {
				usr.removeGroupMemberships(null);
				for (long groupId : user.getGroupIds())
					usr.addGroup(gDao.findById(groupId));
				dao.store(usr);
			}

			return usr.getId();
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public long storeGroup(String sid, WSGroup group) throws Exception {
		checkAdministrator(sid);

		try {
			GroupDAO dao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			Group grp = group.toGroup();
			if (group.getId() != 0) {
				grp = dao.findById(group.getId());
				dao.initialize(grp);
				if (grp.getType() != Group.TYPE_DEFAULT) {
					throw new Exception(String.format("You cannot edit group with id %s because it is a system group",
							grp.getId()));
				}			
				grp.setName(group.getName());
				grp.setDescription(group.getDescription());
				grp.setType(group.getType());				
			}

			if (StringUtils.isEmpty(grp.getName()))
				throw new Exception("Missing mandatory value 'Name'");

			if (group.getUserIds() != null && group.getUserIds().length > 0) {
				
				UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
				for (User usr : grp.getUsers()) {
					usr.removeGroup(grp.getId());
					userDao.store(usr);
				}

				for (long userId : group.getUserIds()) {					
					User user = userDao.findById(userId);
					grp.getUsers().add(user);
					user.addGroup(grp);
					userDao.store(user);
				}
			}

			if (dao.store(grp)) {
				if (group.getInheritGroupId() != null && group.getInheritGroupId().longValue() > 0)
					dao.inheritACLs(grp, group.getInheritGroupId().longValue());
				
				return grp.getId();
			} else
				throw new Exception("Unable to store the group");
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public void deleteUser(String sid, long userId) throws Exception {
		checkAdministrator(sid);

		if (userId == 1)
			throw new Exception("You cannot delete the admin user");

		try {
			UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
			User usr = dao.findById(userId);
			if (usr.getType() == User.TYPE_SYSTEM) {
				throw new Exception("You cannot delete user with id " + usr.getId() + " because it is a system user");
			}
			dao.delete(userId);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception("Unable to delete the user with id " + userId);
		}
	}

	@Override
	public void deleteGroup(String sid, long groupId) throws Exception {
		checkAdministrator(sid);

		if (groupId == 1)
			throw new Exception("You cannot delete the admin group");

		try {
			GroupDAO dao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			Group grp = dao.findById(groupId);
			if (grp.getType() != Group.TYPE_DEFAULT) {
				throw new Exception("You cannot delete group with id " + grp.getId() + " because it is a system group");
			}
			dao.delete(groupId);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception("Unable to delete the group with id " + groupId);
		}
	}

	@Override
	public int changePassword(String sid, long userId, String oldPassword, String newPassword) throws Exception {
		checkAdministrator(sid);

		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user == null)
				throw new Exception("User " + userId + " not found");

			if (oldPassword != null && !CryptUtil.cryptString(oldPassword).equals(user.getPassword())) {
				return 1;
			}

			UserHistory history = null;
			// The password was changed
			user.setDecodedPassword(newPassword);
			user.setPasswordChanged(new Date());
			// Add a user history entry
			history = new UserHistory();
			history.setUser(user);
			history.setEvent(UserEvent.PASSWORDCHANGED.toString());
			history.setComment("");
			user.setRepass("");

			UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);

			boolean stored = dao.store(user, history);

			if (!stored)
				throw new Exception("User not stored");
			return 0;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return 1;
		}
	}

	@Override
	public WSUser getUser(String sid, long userId) throws Exception {
		checkAdministrator(sid);
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user == null)
				return null;

			userDao.initialize(user);
			return WSUser.fromUser(user);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public WSUser getUserByUsername(String sid, String username) throws Exception {
		checkAdministrator(sid);
		try {
			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			User user = userDao.findByUsername(username);

			if (user == null)
				return null;

			userDao.initialize(user);
			return WSUser.fromUser(user);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public WSGroup getGroup(String sid, long groupId) throws Exception {
		checkAdministrator(sid);

		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		Group group = groupDao.findById(groupId);
		if (group == null)
			return null;

		groupDao.initialize(group);
		return WSGroup.fromGroup(group);
	}
}