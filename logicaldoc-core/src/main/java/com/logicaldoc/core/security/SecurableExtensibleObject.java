package com.logicaldoc.core.security;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.core.metadata.ExtensibleObject;

/**
 * A extensible object that also defines security policies
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public abstract class SecurableExtensibleObject extends ExtensibleObject implements Securable {

	private static final long serialVersionUID = 1L;

	private Set<AccessControlEntry> acl = new HashSet<>();

	@Override
	public Set<AccessControlEntry> getAccessControlList() {
		return acl;
	}

	@Override
	public void setAccessControlList(Set<AccessControlEntry> acl) {
		this.acl = acl;
	}

	@Override
	public AccessControlEntry getAccessControlEntry(long groupId) {
		return acl.stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}

	@Override
	public void addAccessControlEntry(AccessControlEntry ace) {
		if (!acl.add(ace)) {
			acl.remove(ace);
			acl.add(ace);
		}
	}
}