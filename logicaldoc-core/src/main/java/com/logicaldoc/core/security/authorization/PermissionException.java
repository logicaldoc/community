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

	private String username;

	/**
	 * Identifier of the security object the permission refers to
	 */
	private String securityObject;

	private String permission;

	public PermissionException(String username, String securityObject, Permission permission) {
		this(username, securityObject, permission.toString());
	}

	public PermissionException(String username, String securityObject, Permission permission, Throwable cause) {
		this(username, securityObject, permission.toString(), cause);
	}

	public PermissionException(String username, String securityObject, String permission) {
		super(buildMessage(username, securityObject, permission));
		this.username = username;
		this.securityObject = securityObject;
		this.permission = permission;
	}

	public PermissionException(String username, String securityObject, String permission, Throwable cause) {
		super(buildMessage(username, securityObject, permission), cause);
		this.username = username;
		this.securityObject = securityObject;
		this.permission = permission;
	}

	public PermissionException() {
	}

	public PermissionException(String message) {
		super(message);
	}

	public PermissionException(Throwable cause) {
		super(cause);
	}

	public PermissionException(String message, Throwable cause) {
		super(message, cause);
	}

	public String getUsername() {
		return username;
	}

	public String getSecurityObject() {
		return securityObject;
	}

	public String getPermission() {
		return permission;
	}

	private static String buildMessage(String username, String securityObject, String permission) {
		return String.format("User %s does not have permission %s on %s", username, securityObject, permission);
	}
}