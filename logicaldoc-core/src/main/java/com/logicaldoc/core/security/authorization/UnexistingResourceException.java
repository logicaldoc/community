package com.logicaldoc.core.security.authorization;

/**
 * Raised when access to an unexisting resource is required
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public class UnexistingResourceException extends AuthorizationException {

	private static final long serialVersionUID = 1L;

	private final String username;

	/**
	 * Identifier of the resource
	 */
	private final String resource;

	public UnexistingResourceException(String username, String resource) {
		this(username, resource, null);
	}

	public UnexistingResourceException(String username, String resource, Throwable cause) {
		super(buildMessage(username, resource), cause);
		this.username = username;
		this.resource = resource;
	}

	public UnexistingResourceException() {
		username = null;
		resource = null;
	}

	public UnexistingResourceException(String message) {
		super(message);
		username = null;
		resource = null;
	}

	public UnexistingResourceException(Throwable cause) {
		super(cause);
		username = null;
		resource = null;
	}

	public UnexistingResourceException(String message, Throwable cause) {
		super(message, cause);
		username = null;
		resource = null;
	}

	public String getUsername() {
		return username;
	}

	public String getSecurityObject() {
		return resource;
	}

	private static String buildMessage(String username, String resource) {
		return String.format("User %s tried to access unexisting resource %s", username, resource);
	}
}