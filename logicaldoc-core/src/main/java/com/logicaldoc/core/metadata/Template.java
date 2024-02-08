package com.logicaldoc.core.metadata;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.SecurableObject;

/**
 * A template collects a set of attributesets ant is itself an extensible
 * object.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class Template extends AbstractAttributeSet implements SecurableObject {

	private static final long serialVersionUID = 1L;

	private String validation;

	private Set<AccessControlEntry> acl = new HashSet<>();

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}

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