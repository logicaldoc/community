package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

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

	private Long maxApiCalls;

	private Long maxTickets;

	private Long maxWorkflows;

	private Long maxForms;
	
	private boolean enabled = true;

	private Date expire;

	private long users;

	// The read-only users
	private long guests;

	private long documents;

	private long size;

	private long sessions;

	private long apiCalls;

	private long tickets;

	private long workflows;

	private long forms;

	private GUIBranding branding;

	private Integer quotaThreshold = null;

	private List<String> quotaAlertRecipients = new ArrayList<>();

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

	public Long getMaxApiCalls() {
		return maxApiCalls;
	}

	public void setMaxApiCalls(Long maxApiCalls) {
		this.maxApiCalls = maxApiCalls;
	}

	public Long getMaxWorkflows() {
		return maxWorkflows;
	}

	public void setMaxWorkflows(Long maxWorkflows) {
		this.maxWorkflows = maxWorkflows;
	}

	public long getWorkflows() {
		return workflows;
	}

	public void setWorkflows(long workflows) {
		this.workflows = workflows;
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

	public boolean isSystem() {
		return id == Constants.TENANT_SYSTEMID;
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
		return expire == null || expire.before(new Date());
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

	public Long getMaxForms() {
		return maxForms;
	}

	public void setMaxForms(Long maxForms) {
		this.maxForms = maxForms;
	}

	public long getForms() {
		return forms;
	}

	public void setForms(long forms) {
		this.forms = forms;
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

	public List<String> getQuotaAlertRecipients() {
		return quotaAlertRecipients;
	}

	public void setQuotaAlertRecipients(List<String> quotaAlertRecipients) {
		this.quotaAlertRecipients = quotaAlertRecipients;
	}

	public void addQuotaAlertRecipient(String recipient) {
		if (!quotaAlertRecipients.contains(recipient))
			quotaAlertRecipients.add(recipient);
	}

	public void removeQuotaAlertRecipient(String recipient) {
		quotaAlertRecipients = quotaAlertRecipients.stream().filter(r -> !r.equals(recipient))
				.collect(Collectors.toList());
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

	public long getApiCalls() {
		return apiCalls;
	}

	public void setApiCalls(long apiCalls) {
		this.apiCalls = apiCalls;
	}

	public Long getMaxTickets() {
		return maxTickets;
	}

	public void setMaxTickets(Long maxTickets) {
		this.maxTickets = maxTickets;
	}

	public long getTickets() {
		return tickets;
	}

	public void setTickets(long tickets) {
		this.tickets = tickets;
	}
}