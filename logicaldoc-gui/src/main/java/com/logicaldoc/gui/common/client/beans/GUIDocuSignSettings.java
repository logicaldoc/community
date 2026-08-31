package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

public class GUIDocuSignSettings implements Serializable {

	private static final long serialVersionUID = 1L;

	private String apiBaseUrl;

	private String callbackUrl;

	private String authBaseUrl;

	private String accountId;

	private String integrationKey;

	private String secretKey;

	private String refreshToken;

	/**
	 * Subject of the email used to notify users about the new envelope
	 */
	private String subject;

	/**
	 * Message of the email used to notify users about the new envelope
	 */
	private String message;

	/**
	 * Optional expiration date, used when creating a new envelope
	 */
	private Date expire;

	/**
	 * Identifiers of documents to include in the envelope
	 */
	private List<Long> documentIds;

	public String getApiBaseUrl() {
		return apiBaseUrl;
	}

	public void setApiBaseUrl(String apiBaseUrl) {
		this.apiBaseUrl = apiBaseUrl;
	}

	public String getAuthBaseUrl() {
		return authBaseUrl;
	}

	public void setAuthBaseUrl(String authBaseUrl) {
		this.authBaseUrl = authBaseUrl;
	}

	public String getAccountId() {
		return accountId;
	}

	public void setAccountId(String accountId) {
		this.accountId = accountId;
	}

	public String getIntegrationKey() {
		return integrationKey;
	}

	public void setIntegrationKey(String integrationKey) {
		this.integrationKey = integrationKey;
	}

	public String getSecretKey() {
		return secretKey;
	}

	public void setSecretKey(String secretKey) {
		this.secretKey = secretKey;
	}

	public String getRefreshToken() {
		return refreshToken;
	}

	public void setRefreshToken(String refreshToken) {
		this.refreshToken = refreshToken;
	}

	public String getCallbackUrl() {
		return callbackUrl;
	}

	public void setCallbackUrl(String callbackUrl) {
		this.callbackUrl = callbackUrl;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Date getExpire() {
		return expire;
	}

	public void setExpire(Date expire) {
		this.expire = expire;
	}

	public List<Long> getDocumentIds() {
		return documentIds;
	}

	public void setDocumentIds(List<Long> documentIds) {
		this.documentIds = documentIds;
	}
}