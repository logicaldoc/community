package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Group. Useful class to create repository Groups.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.1
 */
@XmlType(name = "WSGroup")
public class WSGroup implements Serializable {

	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;
	
	@WSDoc(documented = false)
	public static final long GROUPID_ADMIN = 1;

	@WSDoc(documented = false)
	public static int TYPE_DEFAULT = 0;

	@WSDoc(documented = false)
	public static int TYPE_USER = 1;

	@WSDoc(description = "unique identifier")
	private long id;

	@WSDoc(description = "must be unique")
	private String name = "";

	private String description = "";

	@WSDoc(description = "set it to 0")
	private int type = TYPE_DEFAULT;

	// Optional group from which to import policies at creation time
	@WSDoc(description = "inherit security policies from this referenced group at creation time", required = false)
	private Long inheritGroupId;

	@WSDoc(description = "set of users in this group")
	private long[] userIds = new long[0];

	@WSDoc(description = "the last modified date (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private String lastModified;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public String getDescription() {
		return description;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public long[] getUserIds() {
		return userIds;
	}

	public void setUserIds(long[] userIds) {
		this.userIds = userIds;
	}

	public String getLastModified() {
		return lastModified;
	}

	public void setLastModified(String lastModified) {
		this.lastModified = lastModified;
	}

	public Long getInheritGroupId() {
		return inheritGroupId;
	}

	public void setInheritGroupId(Long inheritGroupId) {
		this.inheritGroupId = inheritGroupId;
	}

	public Group toGroup() {
		Group group = new Group();

		try {
			group.setId(getId());
			group.setName(getName());
			group.setDescription(getDescription());
			group.setType(getType());

			if (getUserIds().length > 0) {
				UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
				Set<User> users = new HashSet<User>();
				for (long userId : getUserIds()) {
					User user = userDao.findById(userId);
					if (user != null && user.getType() == User.TYPE_DEFAULT)
						users.add(user);
				}
				if (users.size() > 0)
					group.setUsers(users);
			}
		} catch (Throwable e) {
			e.printStackTrace();
		}

		return group;
	}

	public static WSGroup fromGroup(Group group) {
		WSGroup wsGroup = new WSGroup();

		try {
			wsGroup.setId(group.getId());
			wsGroup.setName(group.getName());
			wsGroup.setDescription(group.getDescription());
			wsGroup.setType(group.getType());
			wsGroup.setLastModified(WSUtil.convertDateToString(group.getLastModified()));

			if (group.getUsers() != null && group.getUsers().size() > 0) {
				long[] userIds = new long[group.getUsers().size()];
				int i = 0;
				for (User user : group.getUsers()) {
					userIds[i] = user.getId();
					i++;
				}
				wsGroup.setUserIds(userIds);
			}
		} catch (Throwable e) {
			e.printStackTrace();
		}

		return wsGroup;
	}

	@Override
	public boolean equals(Object other) {
		return id == ((WSGroup) other).getId();
	}

	@Override
	public int hashCode() {
		return new Long(id).hashCode();
	}
}