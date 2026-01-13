package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.Set;

/**
 * An object that implements this interface, supports an Access Control List to
 * define security policies. Each element of the ACL is an AccessControlEntry.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 * 
 * @param <T> The exact type or {@link AccessControlEntry}
 */
public interface Secure<T extends AccessControlEntry> extends Serializable {

	public void setAccessControlList(Set<T> acl);

	public Set<T> getAccessControlList();

	public AccessControlEntry getAccessControlEntry(long groupId);

	/**
	 * Retrieves the set of entries related to whatever group within the
	 * specified set.
	 * 
	 * @param groupIds identifiers of groups
	 * 
	 * @return The set of matching entries
	 */
	public Set<T> getAccessControlEntries(Set<Long> groupIds);

	/**
	 * Adds a new entry, replacing a previous one with the same groupId.
	 * 
	 * @param ace the access control entry to add
	 */
	public void addAccessControlEntry(T ace);
}