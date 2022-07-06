package com.logicaldoc.core.security;

/**
 * Simple bean to map a relationship between a user and it's groups
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class UserGroup {

	private long groupId;

	public UserGroup() {
	}

	public UserGroup(long groupId) {
		super();
		this.groupId = groupId;
	}

	public long getGroupId() {
		return groupId;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (groupId ^ (groupId >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		UserGroup other = (UserGroup) obj;
		if (groupId != other.groupId)
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "" + groupId;
	}
}