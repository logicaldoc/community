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

    @Column(name = "ld_usersquota")
    private Integer usersQuota;

    @Column(name = "ld_rousersquota")
    private Integer readOnlyUsersQuota;

    @Column(name = "ld_sessionsquota")
    private Integer sessionsQuota;

    @Column(name = "ld_documentsquota")
    private Long documentsQuota;

    @Column(name = "ld_pagesquota")
    private Long pagesQuota;

    /**
     * Maximum repository size expressed in MB
     */
    @Column(name = "ld_storagequota")
    private Long storageQuota;

    /**
     * Maximum monthly API calls
     */
    @Column(name = "ld_apicallsquota")
    private Long apiCallsQuota;

    @Column(name = "ld_ticketsquota")
    private Long ticketsQuota;

    @Column(name = "ld_workflowsquota")
    private Long workflowsQuota;

    @Column(name = "ld_formsquota")
    private Long formsQuota;

    @Column(name = "ld_reportsquota")
    private Long reportsQuota;

    @Column(name = "ld_stampsquota")
    private Long stampsQuota;

    @Column(name = "ld_importfoldersquota")
    private Long importFoldersQuota;

    @Column(name = "ld_emailaccountsquota")
    private Long emailAccountsQuota;

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
        this.usersQuota = source.usersQuota;
        this.readOnlyUsersQuota = source.readOnlyUsersQuota;
        this.sessionsQuota = source.sessionsQuota;
        this.documentsQuota = source.documentsQuota;
        this.pagesQuota = source.pagesQuota;
        this.enabled = source.enabled;
        this.expire = source.expire;
        this.storageQuota = source.storageQuota;
        this.apiCallsQuota = source.apiCallsQuota;
        this.ticketsQuota = source.ticketsQuota;
        this.workflowsQuota = source.workflowsQuota;
        this.formsQuota = source.formsQuota;
        this.reportsQuota = source.reportsQuota;
        this.stampsQuota = source.stampsQuota;
        this.importFoldersQuota = source.importFoldersQuota;
        this.emailAccountsQuota = source.emailAccountsQuota;
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

    public Integer getUsersQuota() {
        return usersQuota;
    }

    public void setUsersQuota(Integer usersQuota) {
        this.usersQuota = usersQuota;
    }

    public Integer getSessionsQuota() {
        return sessionsQuota;
    }

    public void setSessionsQuota(Integer sessionsQuota) {
        this.sessionsQuota = sessionsQuota;
    }

    public Long getDocumentsQuota() {
        return documentsQuota;
    }

    public void setDocumentsQuota(Long documentsQuota) {
        this.documentsQuota = documentsQuota;
    }

    public Long getStorageQuota() {
        return storageQuota;
    }

    public void setStorageQuota(Long storageQuota) {
        this.storageQuota = storageQuota;
    }

    public Long getApiCallsQuota() {
        return apiCallsQuota;
    }

    public void setApiCallsQuota(Long apiCallsQuota) {
        this.apiCallsQuota = apiCallsQuota;
    }

    public Long getTicketsQuota() {
        return ticketsQuota;
    }

    public void setTicketsQuota(Long ticketsQuota) {
        this.ticketsQuota = ticketsQuota;
    }

    public Long getWorkflowsQuota() {
        return workflowsQuota;
    }

    public void setWorkflowsQuota(Long workflowsQuota) {
        this.workflowsQuota = workflowsQuota;
    }

    public Long getFormsQuota() {
        return formsQuota;
    }

    public void setFormsQuota(Long formsQuota) {
        this.formsQuota = formsQuota;
    }

    public Long getReportsQuota() {
        return reportsQuota;
    }

    public void setReportsQuota(Long reportsQuota) {
        this.reportsQuota = reportsQuota;
    }

    public Long getStampsQuota() {
        return stampsQuota;
    }

    public void setStampsQuota(Long stampsQuota) {
        this.stampsQuota = stampsQuota;
    }

    public Long getImportFoldersQuota() {
        return importFoldersQuota;
    }

    public void setImportFoldersQuota(Long importFoldersQuota) {
        this.importFoldersQuota = importFoldersQuota;
    }

    public Long getEmailAccountsQuota() {
        return emailAccountsQuota;
    }

    public void setEmailAccountsQuota(Long emailAccountsQuota) {
        this.emailAccountsQuota = emailAccountsQuota;
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
            str = "%s,%s".formatted(str, recipient);
        setQuotaAlertRecipients(str);
    }

    public Integer getReadOnlyUsersQuota() {
        return readOnlyUsersQuota;
    }

    public void setReadOnlyUsersQuota(Integer readOnlyUsersQuota) {
        this.readOnlyUsersQuota = readOnlyUsersQuota;
    }

    public Long getPagesQuota() {
        return pagesQuota;
    }

    public void setPagesQuota(Long pagesQuota) {
        this.pagesQuota = pagesQuota;
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