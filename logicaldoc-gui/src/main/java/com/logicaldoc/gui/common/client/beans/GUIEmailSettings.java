package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Email Settings bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUIEmailSettings implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final String SECURITY_NONE = "0";

	public static final String SECURITY_STARTTLS = "1";

	public static final String SECURITY_TLS = "2";

	public static final String SECURITY_SSL = "3";

	public static final String PROTOCOL_SMTP = "smtp";

	public static final String PROTOCOL_SMTP_MICROSOFT365 = "smtpmicrosoft365";

	private String server;

	private String protocol = PROTOCOL_SMTP;

	private boolean secureAuth = false;

	private int port;

	private String username;

	private String pwd;

	/**
	 * In case of OAuth authentication, this field stores the client secret
	 */
	private String clientSecret;

	/**
	 * In case of OAuth authentication, this field stores the client id
	 */
	private String clientId;

	/**
	 * In case of OAuth authentication, this field stores the tenant information
	 */
	private String clientTenant;

	private String connSecurity = SECURITY_NONE;

	private String senderEmail;

	private boolean userAsFrom = true;

	private int foldering = 3;

	private GUIFolder targetFolder;

	public String getServer() {
		return server;
	}

	public void setServer(String server) {
		this.server = server;
	}

	public boolean isSecureAuth() {
		return secureAuth;
	}

	public void setSecureAuth(boolean secureAuth) {
		this.secureAuth = secureAuth;
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPwd() {
		return pwd;
	}

	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	public String getConnSecurity() {
		return connSecurity;
	}

	public void setConnSecurity(String connSecurity) {
		this.connSecurity = connSecurity;
	}

	public String getSenderEmail() {
		return senderEmail;
	}

	public void setSenderEmail(String senderEmail) {
		this.senderEmail = senderEmail;
	}

	public boolean isUserAsFrom() {
		return userAsFrom;
	}

	public void setUserAsFrom(boolean userAsFrom) {
		this.userAsFrom = userAsFrom;
	}

	public int getFoldering() {
		return foldering;
	}

	public void setFoldering(int foldering) {
		this.foldering = foldering;
	}

	public GUIFolder getTargetFolder() {
		return targetFolder;
	}

	public void setTargetFolder(GUIFolder targetFolder) {
		this.targetFolder = targetFolder;
	}

	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	public String getClientSecret() {
		return clientSecret;
	}

	public String getClientId() {
		return clientId;
	}

	public String getClientTenant() {
		return clientTenant;
	}

	public void setClientSecret(String clientSecret) {
		this.clientSecret = clientSecret;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	public void setClientTenant(String clientTenant) {
		this.clientTenant = clientTenant;
	}
}