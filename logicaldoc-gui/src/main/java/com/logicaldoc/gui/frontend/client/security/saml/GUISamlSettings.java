package com.logicaldoc.gui.frontend.client.security.saml;

import java.io.Serializable;

/**
 * A bean to store the SAML settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class GUISamlSettings implements Serializable {

	private static final long serialVersionUID = 1L;

	private boolean enabled = false;

	private String entityId;

	private String certificate;

	private String privateKey;

	private boolean authnRequestSigned = false;

	private String idpMetadata;

	private boolean wantAssertionsEncrypted = false;

	private boolean wantNameIdEncrypted = false;

	private boolean logoutRequestSigned = false;
	
	private boolean logoutResponseSigned = false;
	
	private String signatureAlgorithm = "SHA-256";

	private String username = "username";

	private String firstName = "firstName";

	private String lastName = "lastName";

	private String email = "email";

	private String group = "group";

	private boolean keepLocalMemberships = true;

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getEntityId() {
		return entityId;
	}

	public void setEntityId(String entityId) {
		this.entityId = entityId;
	}

	public String getCertificate() {
		return certificate;
	}

	public void setCertificate(String certificate) {
		this.certificate = certificate;
	}

	public boolean isAuthnRequestSigned() {
		return authnRequestSigned;
	}

	public void setAuthnRequestSigned(boolean authnRequestSigned) {
		this.authnRequestSigned = authnRequestSigned;
	}

	public String getIdpMetadata() {
		return idpMetadata;
	}

	public void setIdpMetadata(String idpMetadata) {
		this.idpMetadata = idpMetadata;
	}

	public String getPrivateKey() {
		return privateKey;
	}

	public void setPrivateKey(String privateKey) {
		this.privateKey = privateKey;
	}

	public boolean isWantAssertionsEncrypted() {
		return wantAssertionsEncrypted;
	}

	public void setWantAssertionsEncrypted(boolean wantAssertionsEncrypted) {
		this.wantAssertionsEncrypted = wantAssertionsEncrypted;
	}

	public boolean isWantNameIdEncrypted() {
		return wantNameIdEncrypted;
	}

	public void setWantNameIdEncrypted(boolean wantNameIdEncrypted) {
		this.wantNameIdEncrypted = wantNameIdEncrypted;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public boolean isKeepLocalMemberships() {
		return keepLocalMemberships;
	}

	public void setKeepLocalMemberships(boolean keepLocalMemberships) {
		this.keepLocalMemberships = keepLocalMemberships;
	}

	public String getSignatureAlgorithm() {
		return signatureAlgorithm;
	}

	public void setSignatureAlgorithm(String signatureAlgorithm) {
		this.signatureAlgorithm = signatureAlgorithm;
	}

	public boolean isLogoutRequestSigned() {
		return logoutRequestSigned;
	}

	public void setLogoutRequestSigned(boolean logoutRequestSigned) {
		this.logoutRequestSigned = logoutRequestSigned;
	}

	public boolean isLogoutResponseSigned() {
		return logoutResponseSigned;
	}

	public void setLogoutResponseSigned(boolean logoutResponseSigned) {
		this.logoutResponseSigned = logoutResponseSigned;
	}
}