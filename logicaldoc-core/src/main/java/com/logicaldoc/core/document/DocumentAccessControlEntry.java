package com.logicaldoc.core.document;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Embeddable;

import com.logicaldoc.core.security.ExtendedAccessControlEntry;
import com.logicaldoc.core.security.Permission;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
@Embeddable
public class DocumentAccessControlEntry extends ExtendedAccessControlEntry {

	private static final long serialVersionUID = 1L;

	public DocumentAccessControlEntry() {
	}

	public DocumentAccessControlEntry(DocumentAccessControlEntry source) {
		super(source);
	}

	public DocumentAccessControlEntry(long groupId) {
		super(groupId);
	}

	@Override
	public Set<Permission> grantedPermissions() {
		HashSet<Permission> granted = new HashSet<>();
		grantedBasicPermissions(granted);

		if (security == 1)
			granted.add(Permission.SECURITY);
		if (sign == 1)
			granted.add(Permission.SIGN);
		if (subscription == 1)
			granted.add(Permission.SUBSCRIPTION);
		if (workflow == 1)
			granted.add(Permission.WORKFLOW);
		if (readingreq == 1)
			granted.add(Permission.READINGREQ);
		if (customid == 1)
			granted.add(Permission.CUSTOMID);
		return granted;
	}

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		super.grantPermissions(permissions);
		security = booleanToInt(permissions.contains(Permission.SECURITY));
		sign = booleanToInt(permissions.contains(Permission.SIGN));
		subscription = booleanToInt(permissions.contains(Permission.SUBSCRIPTION));
		workflow = booleanToInt(permissions.contains(Permission.WORKFLOW));
		readingreq = booleanToInt(permissions.contains(Permission.READINGREQ));
		customid = booleanToInt(permissions.contains(Permission.CUSTOMID));
	}
}
