package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.Constants;

/**
 * This class represents a Tenant, that is a branch of the organization or an
 * organizational unit or whatever other class of organization.
 * 
 * @author Marco Meschieri
 * 
 * @version 6.9
 */
public class GUITenant implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private long tenantId = 0;

	private String name;

	private String displayName;

	private String street;

	private String postalCode;

	private String city;

	private String state;

	private String country;

	private String email;

	private String telephone;

	private int type = 0;

	private String adminUsername = "admin";

	private Integer maxUsers;

	private Integer maxGuests;

	private Integer maxSessions;

	private Long maxRepoDocs;

	/**
	 * Maximum repository size expressed in MB
	 */
	private Long maxRepoSize;

	private boolean enabled = true;

	private Date expire;

	private long users;

	// The read-only users
	private long guests;

	private long documents;

	private long size;

	private long sessions;

	private GUIBranding branding;

	private Integer quotaThreshold = null;

	private String[] quotaAlertRecipients = new String[0];

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getStreet() {
		return street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getTelephone() {
		return telephone;
	}

	public void setTelephone(String telephone) {
		this.telephone = telephone;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return name;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	public String getAdminUsername() {
		return adminUsername;
	}

	public void setAdminUsername(String adminUsername) {
		this.adminUsername = adminUsername;
	}

	public Integer getMaxUsers() {
		return maxUsers;
	}

	public void setMaxUsers(Integer maxUsers) {
		this.maxUsers = maxUsers;
	}

	public Integer getMaxSessions() {
		return maxSessions;
	}

	public void setMaxSessions(Integer maxSessions) {
		this.maxSessions = maxSessions;
	}

	public Long getMaxRepoDocs() {
		return maxRepoDocs;
	}

	public void setMaxRepoDocs(Long maxRepoDocs) {
		this.maxRepoDocs = maxRepoDocs;
	}

	public Long getMaxRepoSize() {
		return maxRepoSize;
	}

	public void setMaxRepoSize(Long maxRepoSize) {
		this.maxRepoSize = maxRepoSize;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public boolean isDefault() {
		return id == Constants.TENANT_DEFAULTID;
	}

	public Date getExpire() {
		return expire;
	}

	public void setExpire(Date expire) {
		this.expire = expire;
	}

	public boolean isAvailable() {
		if (!enabled)
			return false;
		if (expire != null && expire.before(new Date()))
			return false;
		return true;
	}

	public long getUsers() {
		return users;
	}

	public void setUsers(long users) {
		this.users = users;
	}

	public long getDocuments() {
		return documents;
	}

	public void setDocuments(long documents) {
		this.documents = documents;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public long getSessions() {
		return sessions;
	}

	public void setSessions(long sessions) {
		this.sessions = sessions;
	}

	public GUIBranding getBranding() {
		return branding;
	}

	public void setBranding(GUIBranding branding) {
		this.branding = branding;
	}

	public Integer getQuotaThreshold() {
		return quotaThreshold;
	}

	public void setQuotaThreshold(Integer quotaThreshold) {
		this.quotaThreshold = quotaThreshold;
	}

	public String[] getQuotaAlertRecipients() {
		return quotaAlertRecipients;
	}

	public String getQuotaAlertRecipientsAsString() {
		if (quotaAlertRecipients == null || quotaAlertRecipients.length == 0)
			return null;
		StringBuilder str = new StringBuilder();
		for (String rec : quotaAlertRecipients) {
			if (!str.toString().isEmpty())
				str.append(",");
			str.append(rec.trim());
		}
		return str.toString();
	}

	public void setQuotaAlertRecipients(String[] quotaAlertRecipients) {
		this.quotaAlertRecipients = quotaAlertRecipients;
	}

	public void clearQuotaAlertRecipients() {
		this.quotaAlertRecipients = new String[] {};
	}

	public void addQuotaAlertRecipient(String recipient) {
		String[] tmp = null;
		if (quotaAlertRecipients != null) {
			tmp = new String[quotaAlertRecipients.length + 1];

			int i = 0;
			for (String tg : quotaAlertRecipients) {
				// Skip if the tag already exists
				if (tg.equals(recipient))
					return;
				tmp[i++] = tg;
			}
			tmp[i] = recipient;
			quotaAlertRecipients = tmp;
		} else
			quotaAlertRecipients = new String[] { recipient };
	}

	public void removeQuotaAlertRecipient(String recipient) {
		if (quotaAlertRecipients == null || quotaAlertRecipients.length == 0)
			return;

		String[] tmp = new String[quotaAlertRecipients.length - 1];
		int i = 0;
		for (String tg : quotaAlertRecipients) {
			if (!tg.equals(recipient) && tmp.length > 0)
				tmp[i++] = tg;
		}
		quotaAlertRecipients = tmp;
	}

	public Integer getMaxGuests() {
		return maxGuests;
	}

	public void setMaxGuests(Integer maxGuests) {
		this.maxGuests = maxGuests;
	}

	public long getGuests() {
		return guests;
	}

	public void setGuests(long guests) {
		this.guests = guests;
	}
}