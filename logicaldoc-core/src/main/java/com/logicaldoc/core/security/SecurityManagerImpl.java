package com.logicaldoc.core.security;

import java.util.HashSet;
import java.util.Set;

import javax.annotation.Resource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;

/**
 * Basic implementation of <code>SecurityManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@Component("SecurityManager")
public class SecurityManagerImpl implements SecurityManager {

	protected static Logger log = LoggerFactory.getLogger(SecurityManagerImpl.class);

	@Resource(name = "UserDAO")
	protected UserDAO userDAO;

	@Resource(name = "GroupDAO")
	protected GroupDAO groupDAO;

	@Resource(name = "MenuDAO")
	protected MenuDAO menuDAO;

	@Resource(name = "FolderDAO")
	protected FolderDAO folderDAO;

	@Resource(name = "DocumentDAO")
	protected DocumentDAO documentDAO;

	private SecurityManagerImpl() {
	}

	public void setMenuDAO(MenuDAO menuDAO) {
		this.menuDAO = menuDAO;
	}

	public void setGroupDAO(GroupDAO groupDAO) {
		this.groupDAO = groupDAO;
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	@Override
	public Set<Group> getAllowedGroups(long menuId) {
		Set<Group> groups = new HashSet<>();
		try {
			Menu menu = menuDAO.findById(menuId);

			for (AccessControlEntry mg : menu.getAccessControlList()) {
				Group group = groupDAO.findById(mg.getGroupId());
				if (!groups.contains(group))
					groups.add(groupDAO.findById(mg.getGroupId()));
			}
			Group admin = groupDAO.findById(Group.GROUPID_ADMIN);
			if (!groups.contains(admin))
				groups.add(admin);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return groups;
	}

	@Override
	public boolean isMemberOf(long userId, long groupId) {
		try {
			return userDAO.queryForInt(
					"select count(*) from ld_usergroup where ld_userid=" + userId + " and ld_groupid=" + groupId) > 0;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	@Override
	public boolean isMemberOf(long userId, String groupName) {
		User user = null;
		try {
			user = userDAO.findById(userId, true);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (user == null)
			return false;
		userDAO.initialize(user);
		for (Group group : user.getGroups())
			if (group.getName().equals(groupName))
				return true;
		return false;
	}

	@Override
	public boolean isWriteEnabled(long docId, long userId) throws PersistenceException {
		return documentDAO.isWriteEnabled(docId, userId);
	}

	@Override
	public boolean isReadEnabled(long docId, long userId) throws PersistenceException {
		return documentDAO.isReadEnabled(docId, userId);
	}

	@Override
	public boolean isPrintEnabled(long docId, long userId) throws PersistenceException {
		return documentDAO.isPrintEnabled(docId, userId);
	}

	@Override
	public boolean isDownloadEnabled(long docId, long userId) throws PersistenceException {
		return documentDAO.isDownloadEnabled(docId, userId);
	}

	@Override
	public boolean isPermissionEnabled(Permission permission, long docId, long userId) throws PersistenceException {
		return documentDAO.isPermissionEnabled(permission, docId, userId);
	}

	@Override
	public Set<Permission> getEnabledPermissions(long docId, long userId) throws PersistenceException {
		return documentDAO.getEnabledPermissions(docId, userId);
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	public void setDocumentDAO(DocumentDAO documentDAO) {
		this.documentDAO = documentDAO;
	}
}