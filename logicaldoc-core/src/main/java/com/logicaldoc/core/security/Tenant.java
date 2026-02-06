package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistentObject;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * This class represents a Tenant, that is a branch of the organization or an
 * organizational unit or whatever other class of organization.
 * 
 * @author Marco Meschieri
 * 
 * @version 6.9
 */
@Entity
@Table(name = "ld_tenant")
@Cacheable
public class Tenant extends PersistentObject implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final long SYSTEM_ID = -1L;

	public static final String SYSTEM_NAME = "system";

	public static final long DEFAULT_ID = 1L;

	public static final String DEFAULT_NAME = "default";

	public static final int DEFAULT_TYPE = 0;

	@Column(name = "ld_name", length = 255, nullable = false)
	private String name;

	@Column(name = "ld_displayname", length = 4000)
	private String displayName;

	@Column(name = "ld_enabled", nullable = false)
	private boolean enabled = true;

	@Column(name = "ld_expire", columnDefinition = "DATETIME(3)")
	private Date expire;

	@Column(name = "ld_street", length = 255)
	private String street;

	@Column(name = "ld_postalcode", length = 255)
	private String postalCode;

	@Column(name = "ld_city", length = 255)
	private String city;

	@Column(name = "ld_country", length = 255)
	private String country;

	@Column(name = "ld_state", length = 255)
	private String state;

	@Column(name = "ld_email", length = 255)
	private String email;

	@Column(name = "ld_telephone", length = 255)
	private String telephone;

	@Column(name = "ld_type", nullable = false)
	private int type = DEFAULT_TYPE;

	@Column(name = "ld_maxusers", nullable = true)
	private Integer maxUsers;

	@Column(name = "ld_maxguests", nullable = true)
	private Integer maxGuests;

	@Column(name = "ld_maxsessions", nullable = true)
	private Integer maxSessions;

	@Column(name = "ld_maxdocuments", nullable = true)
	private Long maxDocuments;
	
	/**
	 * Maximum repository size expressed in MB
	 */
	@Column(name = "ld_maxreposize", nullable = true)
	private Long maxRepoSize;

	/**
	 * Maximum monthly API calls
	 */
	@Column(name = "ld_maxapicalls", nullable = true)
	private Long maxApiCalls;

	@Column(name = "ld_maxtickets", nullable = true)
	private Long maxTickets;

	@Column(name = "ld_maxworkflows", nullable = true)
	private Long maxWorkflows;

	@Column(name = "ld_maxforms", nullable = true)
	private Long maxForms;

	@Column(name = "ld_maxreports", nullable = true)
	private Long maxReports;

	@Column(name = "ld_maxstamps", nullable = true)
	private Long maxStamps;


	@Column(name = "ld_qthreshold")
	private Integer quotaThreshold = null;

	@Column(name = "ld_qrecipients", length = 1000)
	private String quotaAlertRecipients = null;

	public Tenant() {
	}

	public Tenant(Tenant source) {
		this.name = source.name;
		this.displayName = source.displayName;
		this.setCreation(source.getCreation());
		this.street = source.street;
		this.postalCode = source.postalCode;
		this.city = source.city;
		this.state = source.state;
		this.country = source.country;
		this.email = source.email;
		this.telephone = source.telephone;
		this.type = source.type;
		this.maxUsers = source.maxUsers;
		this.maxGuests = source.maxGuests;
		this.maxSessions = source.maxSessions;
		this.maxDocuments = source.maxDocuments;
		this.enabled = source.enabled;
		this.expire = source.expire;
		this.maxRepoSize = source.maxRepoSize;
		this.maxApiCalls = source.maxApiCalls;
		this.maxTickets = source.maxTickets;
		this.maxWorkflows = source.maxWorkflows;
		this.maxForms = source.maxForms;
		this.maxReports = source.maxReports;
		this.maxStamps = source.maxStamps;
		this.quotaThreshold = source.quotaThreshold;
		this.quotaAlertRecipients = source.quotaAlertRecipients;

		setId(source.getId());
		setTenantId(source.getTenantId());
	}

	public boolean isDefault() {
		return id == DEFAULT_ID;
	}

	public boolean isSystem() {
		return id == SYSTEM_ID;
	}

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

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	@Override
	public String toString() {
		return displayName != null ? displayName : name;
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

	public Long getMaxDocuments() {
		return maxDocuments;
	}

	public void setMaxDocuments(Long maxDocuments) {
		this.maxDocuments = maxDocuments;
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

	public Long getMaxTickets() {
		return maxTickets;
	}

	public void setMaxTickets(Long maxTickets) {
		this.maxTickets = maxTickets;
	}

	public Long getMaxWorkflows() {
		return maxWorkflows;
	}

	public void setMaxWorkflows(Long maxWorkflows) {
		this.maxWorkflows = maxWorkflows;
	}

	public Long getMaxForms() {
		return maxForms;
	}

	public void setMaxForms(Long maxForms) {
		this.maxForms = maxForms;
	}

	public Long getMaxReports() {
		return maxReports;
	}

	public void setMaxReports(Long maxReports) {
		this.maxReports = maxReports;
	}
	
	public Long getMaxStamps() {
		return maxStamps;
	}

	public void setMaxStamps(Long maxStamps) {
		this.maxStamps = maxStamps;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * Check if the tenant is enabled and not expired
	 * 
	 * @return if it is available
	 */
	public boolean isAvailable() {
		if (!enabled)
			return false;
		return expire == null || expire.before(new Date());
	}

	public Date getExpire() {
		return expire;
	}

	public void setExpire(Date expire) {
		this.expire = expire;
	}

	public Integer getQuotaThreshold() {
		return quotaThreshold;
	}

	public void setQuotaThreshold(Integer quotaThreshold) {
		this.quotaThreshold = quotaThreshold;
	}

	public String getQuotaAlertRecipients() {
		return quotaAlertRecipients;
	}

	public void setQuotaAlertRecipients(String quotaAlertRecipients) {
		this.quotaAlertRecipients = quotaAlertRecipients;
	}

	public List<String> getQuotaAlertRecipientsAsList() {
		List<String> list = new ArrayList<>();
		if (!StringUtils.isEmpty(getQuotaAlertRecipients())) {
			StringTokenizer st = new StringTokenizer(getQuotaAlertRecipients(), ",", false);
			while (st.hasMoreTokens())
				list.add(st.nextToken().trim());
		}
		return list;
	}

	public void addQuotaAlertRecipient(String recipient) {
		if (StringUtils.isEmpty(recipient))
			return;
		String str = getQuotaAlertRecipients();
		if (StringUtils.isEmpty(str))
			str = recipient;
		else
			str += "," + recipient;
		setQuotaAlertRecipients(str);
	}

	public Integer getMaxGuests() {
		return maxGuests;
	}

	public void setMaxGuests(Integer maxGuests) {
		this.maxGuests = maxGuests;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Tenant other = (Tenant) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}