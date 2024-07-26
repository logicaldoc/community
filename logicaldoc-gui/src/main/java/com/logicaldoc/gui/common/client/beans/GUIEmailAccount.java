package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Model of an email account.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIEmailAccount implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;

	public static final String PROVIDER_POP3 = "pop3";

	public static final String PROVIDER_IMAP = "imap";

	public static final int FORMAT_MULTIPLE = 0;

	public static final int FORMAT_EML = 1;

	private String mailAddress;

	private String provider = PROVIDER_POP3;

	private String host;

	private int port = 110;

	private String username;

	private String password;

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

	private String mailFolder;

	// Comma separated list of allowed extensions
	private String includes = "*.pdf,*.doc,*.txt";

	private String excludes = "";

	private String language = "";

	private boolean deleteFromMailbox = false;

	private int enabled = 1;

	private boolean ssl = false;

	private int foldering = 2;

	private Date startDate;

	/**
	 * This attribute defines the EmailAccount store format.
	 * 
	 * @see #FORMAT_MULTIPLE
	 * @see #FORMAT_EML
	 */
	private int format = FORMAT_EML;

	private GUIFolder target = null;

	private String type = "default";

	private List<GUIEmailRule> rules = new ArrayList<>();

	private String automation;

	private String automationAfter;

	public GUIEmailAccount() {
		super();
		target = null;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getMailAddress() {
		return mailAddress;
	}

	public void setMailAddress(String mailAddress) {
		this.mailAddress = mailAddress;
	}

	public String getProvider() {
		return provider;
	}

	public void setProvider(String provider) {
		this.provider = provider;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
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

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getMailFolder() {
		return mailFolder;
	}

	public void setMailFolder(String mailFolder) {
		this.mailFolder = mailFolder;
	}

	public String getIncludes() {
		return includes;
	}

	public void setIncludes(String includes) {
		this.includes = includes;
	}

	public String getExcludes() {
		return excludes;
	}

	public void setExcludes(String excludes) {
		this.excludes = excludes;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public int getFormat() {
		return format;
	}

	public void setFormat(int format) {
		this.format = format;
	}

	public GUIFolder getTarget() {
		return target;
	}

	public void setTarget(GUIFolder target) {
		this.target = target;
	}

	public boolean isDeleteFromMailbox() {
		return deleteFromMailbox;
	}

	public void setDeleteFromMailbox(boolean deleteFromMailbox) {
		this.deleteFromMailbox = deleteFromMailbox;
	}

	public boolean isSsl() {
		return ssl;
	}

	public void setSsl(boolean ssl) {
		this.ssl = ssl;
	}

	public List<GUIEmailRule> getRules() {
		return rules;
	}

	public void setRules(List<GUIEmailRule> rules) {
		this.rules = rules;
	}

	public int getFoldering() {
		return foldering;
	}

	public void setFoldering(int foldering) {
		this.foldering = foldering;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public String getAutomationAfter() {
		return automationAfter;
	}

	public void setAutomationAfter(String automationAfter) {
		this.automationAfter = automationAfter;
	}

	public String getClientSecret() {
		return clientSecret;
	}

	public String getClientId() {
		return clientId;
	}

	public void setClientSecret(String clientSecret) {
		this.clientSecret = clientSecret;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	public String getClientTenant() {
		return clientTenant;
	}

	public void setClientTenant(String clientTenant) {
		this.clientTenant = clientTenant;
	}
}