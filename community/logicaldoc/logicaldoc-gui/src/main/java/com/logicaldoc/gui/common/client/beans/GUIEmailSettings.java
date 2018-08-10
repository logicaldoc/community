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

	public static final String SECURITY_TLS_IF_AVAILABLE = "1";

	public static final String SECURITY_TLS = "2";

	public static final String SECURITY_SSL = "3";

	private String smtpServer;

	private boolean secureAuth = false;

	private int port;

	private String username;

	private String pwd;

	private String connSecurity = SECURITY_NONE;

	private String senderEmail;

	private boolean userAsFrom = true;

	public String getSmtpServer() {
		return smtpServer;
	}

	public void setSmtpServer(String smtpServer) {
		this.smtpServer = smtpServer;
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
}