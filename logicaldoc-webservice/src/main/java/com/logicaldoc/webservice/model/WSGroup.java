package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.xml.bind.annotation.XmlType;

import org.apache.commons.collections.CollectionUtils;

import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.time.DateUtil;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Group. Useful class to create repository Groups.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@XmlType(name = "WSGroup")
public class WSGroup implements Serializable {

	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(documented = false)
	public static final long GROUPID_ADMIN = 1;

	@WSDoc(documented = false)
	public static final int TYPE_DEFAULT = 0;

	@WSDoc(documented = false)
	public static final int TYPE_USER = 1;

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
	private List<Long> userIds = new ArrayList<>();

	@WSDoc(description = "the last modified date (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private String lastModified;

	@WSDoc(description = "where the group was created, 'local' indicates it was created in the local database", required = false)
	private String source = "local";

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

	public List<Long> getUserIds() {
		return userIds;
	}

	public void setUserIds(List<Long> userIds) {
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

			if (CollectionUtils.isNotEmpty(userIds)) {
				UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
				Set<User> users = new HashSet<>();
				for (long userId : getUserIds()) {
					User user = userDao.findById(userId);
					if (user != null && user.getType() == User.TYPE_DEFAULT)
						users.add(user);
				}
				if (CollectionUtils.isNotEmpty(users))
					group.setUsers(users);
			}
		} catch (Exception e) {
			// Nothing to do
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
			wsGroup.setLastModified(DateUtil.format(group.getLastModified()));
			wsGroup.setSource(group.getSource());
			wsGroup.setUserIds(group.getUsers().stream().map(u -> u.getId()).collect(Collectors.toList()));
		} catch (Exception e) {
			// Nothing to do
		}

		return wsGroup;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;

		if (this.getClass() != obj.getClass())
			return false;

		return id == ((WSGroup) obj).getId();
	}

	@Override
	public int hashCode() {
		return Long.valueOf(id).hashCode();
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}
}