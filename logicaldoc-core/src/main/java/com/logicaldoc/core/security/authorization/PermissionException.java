package com.logicaldoc.core.security.authorization;

import com.logicaldoc.core.security.Permission;

/**
 * Raised when a permission is not allowed
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class PermissionException extends AuthorizationException {

	private static final long serialVersionUID = 1L;

	private final String username;

	/**
	 * Identifier of the security object the permission refers to
	 */
	private final String securityObject;

	private final Permission permission;

	public PermissionException(String username, String securityObject, Permission permission) {
		this(username, securityObject, permission, null);

	}

	public PermissionException(String username, String securityObject, Permission permission, Throwable cause) {
		super(buildMessage(username, securityObject, permission), cause);
		this.username = username;
		this.securityObject = securityObject;
		this.permission = permission;
	}

	public PermissionException() {
		username = null;
		securityObject = null;
		permission = null;
	}

	public PermissionException(String message) {
		super(message);
		username = null;
		securityObject = null;
		permission = null;
	}

	public PermissionException(Throwable cause) {
		super(cause);
		username = null;
		securityObject = null;
		permission = null;
	}

	public PermissionException(String message, Throwable cause) {
		super(message, cause);
		username = null;
		securityObject = null;
		permission = null;
	}

	public String getUsername() {
		return username;
	}

	public String getSecurityObject() {
		return securityObject;
	}

	public Permission getPermission() {
		return permission;
	}

	private static String buildMessage(String username, String securityObject, Permission permission) {
		return String.format("User %s does not have permission %s on %s", username, permission.name(), securityObject);
	}
}