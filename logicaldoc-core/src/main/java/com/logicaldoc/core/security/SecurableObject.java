package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.Set;

/**
 * An object that defines an Access Control List
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 *
 */
public interface SecurableObject extends Serializable {

	public void setAccessControlList(Set<AccessControlEntry> acl);

	public Set<AccessControlEntry> getAccessControlList();

	public AccessControlEntry getAccessControlEntry(long groupId);

	/**
	 * Adds a new element, replacing a previous one with the same groupId.
	 * 
	 * @param ace the access control entry to add
	 */
	public void addAccessControlEntry(AccessControlEntry ace);
}
