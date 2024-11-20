package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.WorkingTime;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
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
	public List<WSUser> listUsers(String sid, String group)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		try {
			List<WSUser> users = collectUsers(group, user);

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

			return users;
		} catch (Exception t) {
			throw new PersistenceException(t.getMessage());
		}
	}

	private List<WSUser> collectUsers(String group, User user) throws PersistenceException {
		List<WSUser> users = new ArrayList<>();
		UserDAO dao = Context.get().getBean(UserDAO.class);
		if (StringUtils.isEmpty(group)) {
			for (User usr : dao.findAll(user.getTenantId())) {
				dao.initialize(user);
				if (usr.getType() != User.TYPE_SYSTEM)
					users.add(WSUser.fromUser(usr));
			}
		} else {
			GroupDAO gDao = Context.get().getBean(GroupDAO.class);
			Group grp = gDao.findByName(group, user.getTenantId());
			gDao.initialize(grp);
			for (User usr : grp.getUsers()) {
				dao.initialize(user);
				if (usr.getType() != User.TYPE_SYSTEM)
					users.add(WSUser.fromUser(usr));
			}
		}
		return users;
	}

	@Override
	public List<WSGroup> listGroups(String sid) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		User user = validateSession(sid);

		try {
			List<WSGroup> groups = new ArrayList<>();
			GroupDAO dao = Context.get().getBean(GroupDAO.class);
			for (Group grp : dao.findAll(user.getTenantId())) {
				if (grp.getType() == Group.TYPE_DEFAULT) {
					dao.initialize(grp);
					groups.add(WSGroup.fromGroup(grp));
				}
			}
			return groups;
		} catch (Exception t) {
			throw new PersistenceException(t.getMessage());
		}
	}

	@Override
	public long storeUser(String sid, WSUser wsUser) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		User sessionUser = validateSession(sid);

		try {
			GroupDAO gDao = Context.get().getBean(GroupDAO.class);
			UserDAO dao = Context.get().getBean(UserDAO.class);
			User usr = wsUser.toUser();
			usr.setTenantId(sessionUser.getTenantId());

			if (wsUser.getId() != 0) {
				usr = dao.findById(wsUser.getId());
				if (usr.getType() == User.TYPE_SYSTEM)
					throw new PermissionException(
							"You cannot edit user with id " + usr.getId() + " because it is a system user");
				dao.initialize(usr);

				usr.setCity(wsUser.getCity());
				usr.setCountry(wsUser.getCountry());
				usr.setEmail(wsUser.getEmail());
				usr.setEmailSignature(wsUser.getEmailSignature());
				usr.setEmail2(wsUser.getEmail2());
				usr.setEmailSignature2(wsUser.getEmailSignature2());
				usr.setFirstName(wsUser.getFirstName());
				usr.setName(wsUser.getName());
				usr.setLanguage(wsUser.getLanguage());
				usr.setPostalcode(wsUser.getPostalcode());
				usr.setState(wsUser.getState());
				usr.setStreet(wsUser.getStreet());
				usr.setTelephone(wsUser.getTelephone());
				usr.setTelephone2(wsUser.getTelephone2());
				usr.setCompany(wsUser.getCompany());
				usr.setDepartment(wsUser.getDepartment());
				usr.setOrganizationalUnit(wsUser.getOrganizationalUnit());
				usr.setBuilding(wsUser.getBuilding());
				usr.setUsername(wsUser.getUsername());
				usr.setEnabled(wsUser.getEnabled());
				usr.setPasswordExpires(wsUser.getPasswordExpires());
				usr.setQuota(wsUser.getQuota());
				usr.setType(wsUser.getType());
				usr.setSource(wsUser.getSource());
				usr.setDateFormat(wsUser.getDateFormat());
				usr.setDateFormatShort(wsUser.getDateFormatShort());
				usr.setDateFormatLong(wsUser.getDateFormatLong());
				usr.setKey(wsUser.getKey());
				usr.setSecondFactor(wsUser.getSecondFactor());
				usr.setTimeZone(wsUser.getTimeZone());
				usr.setExpire(WSUtil.convertStringToDate(wsUser.getExpire()));
				usr.setEnforceWorkingTime(wsUser.getEnforceWorkingTime());
				usr.setMaxInactivity(wsUser.getMaxInactivity());

				if (CollectionUtils.isNotEmpty(wsUser.getWorkingTimes()))
					for (WSWorkingTime wswt : wsUser.getWorkingTimes()) {
						WorkingTime wt = new WorkingTime();
						BeanUtils.copyProperties(wt, wswt);
						usr.getWorkingTimes().add(wt);
					}
			} else {
				usr.setDecodedPassword(wsUser.getDecodedPassword());
			}

			validateMandatoryFields(usr);

			dao.store(usr);

			if (CollectionUtils.isNotEmpty(wsUser.getGroupIds())) {
				usr.removeGroupMemberships(null);
				for (long groupId : wsUser.getGroupIds())
					usr.addGroup(gDao.findById(groupId));
				dao.store(usr);
			}

			return usr.getId();
		} catch (Exception t) {
			throw new PersistenceException(t.getMessage());
		}
	}

	private void validateMandatoryFields(User usr) throws PersistenceException {
		if (StringUtils.isEmpty(usr.getUsername()))
			throw new PersistenceException("Missing mandatory value 'UserName'");
		else if (StringUtils.isEmpty(usr.getEmail()))
			throw new PersistenceException("Missing mandatory value 'Email'");
		else if (StringUtils.isEmpty(usr.getName()))
			throw new PersistenceException("Missing mandatory value 'Name'");
		else if (StringUtils.isEmpty(usr.getFirstName()))
			throw new PersistenceException("Missing mandatory value 'FirstName'");
	}

	@Override
	public long storeGroup(String sid, WSGroup group) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);

		try {
			GroupDAO dao = Context.get().getBean(GroupDAO.class);
			Group grp = group.toGroup();
			if (group.getId() != 0) {
				grp = dao.findById(group.getId());
				dao.initialize(grp);
				if (grp.getType() != Group.TYPE_DEFAULT) {
					throw new PermissionException(String
							.format("You cannot edit group with id %s because it is a system group", grp.getId()));
				}
				grp.setName(group.getName());
				grp.setDescription(group.getDescription());
				grp.setType(group.getType());
			}

			if (StringUtils.isEmpty(grp.getName()))
				throw new PersistenceException("Missing mandatory value 'Name'");

			if (CollectionUtils.isNotEmpty(group.getUserIds())) {

				UserDAO userDao = Context.get().getBean(UserDAO.class);
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

			dao.store(grp);
			if (group.getInheritGroupId() != null && group.getInheritGroupId().longValue() > 0)
				dao.inheritACLs(grp, group.getInheritGroupId().longValue());

			return grp.getId();
		} catch (Exception t) {
			throw new PersistenceException(t.getMessage());
		}
	}

	@Override
	public void deleteUser(String sid, long userId)
			throws WebserviceException, PersistenceException, PermissionException {
		checkAdministrator(sid);

		if (userId == 1)
			throw new PermissionException("You cannot delete the admin user");

		try {
			UserDAO dao = Context.get().getBean(UserDAO.class);
			User usr = dao.findById(userId);
			if (usr.getType() == User.TYPE_SYSTEM) {
				throw new PermissionException(
						"You cannot delete user with id " + usr.getId() + " because it is a system user");
			}
			dao.delete(userId);
		} catch (Exception t) {
			throw new PersistenceException("Unable to delete the user with id " + userId);
		}
	}

	@Override
	public void deleteGroup(String sid, long groupId)
			throws PermissionException, PersistenceException, WebserviceException {
		checkAdministrator(sid);

		if (groupId == 1)
			throw new PermissionException("You cannot delete the admin group");

		try {
			GroupDAO dao = Context.get().getBean(GroupDAO.class);
			Group grp = dao.findById(groupId);
			if (grp.getType() != Group.TYPE_DEFAULT) {
				throw new PermissionException(
						"You cannot delete group with id " + grp.getId() + " because it is a system group");
			}
			dao.delete(groupId);
		} catch (Exception t) {
			throw new PersistenceException("Unable to delete the group with id " + groupId);
		}
	}

	@Override
	public int changePassword(String sid, long userId, String oldPassword, String newPassword)
			throws WebserviceException, PersistenceException {
		checkAdministrator(sid);

		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user == null)
				throw new WebserviceException("User " + userId + " not found");

			if (oldPassword != null && !CryptUtil.encryptSHA256(oldPassword).equals(user.getPassword())
					&& !CryptUtil.encryptSHA(oldPassword).equals(user.getPassword())) {
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

			UserDAO dao = Context.get().getBean(UserDAO.class);

			dao.store(user, history);

			return 0;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return 1;
		}
	}

	@Override
	public WSUser getUser(String sid, long userId) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findById(userId);
			if (user == null)
				return null;

			userDao.initialize(user);
			return WSUser.fromUser(user);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public WSUser getUserByUsername(String sid, String username) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		try {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findByUsername(username);

			if (user == null)
				return null;

			userDao.initialize(user);
			return WSUser.fromUser(user);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public WSGroup getGroup(String sid, long groupId) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);

		GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
		Group group = groupDao.findById(groupId);
		if (group == null)
			return null;

		groupDao.initialize(group);
		return WSGroup.fromGroup(group);
	}
}