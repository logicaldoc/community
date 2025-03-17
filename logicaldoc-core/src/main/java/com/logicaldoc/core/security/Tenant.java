package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.StringTokenizer;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.apache.commons.lang.StringUtils;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;

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
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
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
	private int enabled = 1;
	
	@Column(name = "ld_expire")
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

	@Column(name = "ld_maxrepodocs", nullable = true)
	private Long maxRepoDocs;

	/**
	 * Maximum repository size expressed in MB
	 */
	@Column(name = "ld_maxreposize", nullable = true)
	private Long maxRepoSize;
	
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
		this.maxRepoDocs = source.maxRepoDocs;
		this.enabled = source.enabled;
		this.expire = source.expire;
		this.maxRepoSize = source.maxRepoSize;
		this.quotaThreshold = source.quotaThreshold;
		this.quotaAlertRecipients = source.quotaAlertRecipients;

		setId(source.getId());
		setTenantId(source.getTenantId());
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

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	/**
	 * Check if the tenant is enabled and not expired
	 * 
	 * @return if it is available
	 */
	public boolean isAvailable() {
		if (enabled == 0)
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