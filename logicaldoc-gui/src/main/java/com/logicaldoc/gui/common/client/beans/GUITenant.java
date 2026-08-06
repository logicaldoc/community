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

    private Long regularUsersQuota;

    private Long readonlyUsersQuota;

    private Long sessionsQuota;

    private Long documentsQuota;

    private Long pagesQuota;

    /**
     * Maximum repository size expressed in MB
     */
    private Long storageQuota;

    private Long apiCallsQuota;

    private Long ticketsQuota;

    private Long workflowsQuota;

    private Long formsQuota;

    private Long reportsQuota;

    private Long stampsQuota;

    private Long importFoldersQuota;

    private Integer quotaThreshold = null;

    private Long maxEmailAccounts;

    private boolean enabled = true;

    private Date expire;

    private long regularUsers;

    // The read-only users
    private long readonlyUsers;

    private long documents;

    private long storage;

    private long pages;

    private long sessions;

    private long apiCalls;

    private long tickets;

    private long workflows;

    private long forms;

    private long reports;

    private long stamps;

    private long importFolders;

    private long emailAccounts;

    private GUIBranding branding;

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

    public Long getRegularUsersQuota() {
        return regularUsersQuota;
    }

    public void setRegularUsersQuota(Long regularUsersQuota) {
        this.regularUsersQuota = regularUsersQuota;
    }

    public Long getSessionsQuota() {
        return sessionsQuota;
    }

    public void setSessionsQuota(Long sessionsQuota) {
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

    public Long getWorkflowsQuota() {
        return workflowsQuota;
    }

    public void setWorkflowsQuota(Long workflowsQuota) {
        this.workflowsQuota = workflowsQuota;
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

    public long getRegularUsers() {
        return regularUsers;
    }

    public void setRegularUsers(long regularUsers) {
        this.regularUsers = regularUsers;
    }

    public long getDocuments() {
        return documents;
    }

    public void setDocuments(long documents) {
        this.documents = documents;
    }

    public long getSessions() {
        return sessions;
    }

    public void setSessions(long sessions) {
        this.sessions = sessions;
    }

    public Long getFormsQuota() {
        return formsQuota;
    }

    public void setFormsQuota(Long formsQuota) {
        this.formsQuota = formsQuota;
    }

    public long getForms() {
        return forms;
    }

    public void setForms(long forms) {
        this.forms = forms;
    }

    public long getReports() {
        return reports;
    }

    public void setReports(long reports) {
        this.reports = reports;
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

    public long getStamps() {
        return stamps;
    }

    public void setStamps(long stamps) {
        this.stamps = stamps;
    }

    public long getImportFolders() {
        return importFolders;
    }

    public void setImportFolders(long importFolders) {
        this.importFolders = importFolders;
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

    public Long getReadonlyUsersQuota() {
        return readonlyUsersQuota;
    }

    public void setReadonlyUsersQuota(Long readonlyUsersQuota) {
        this.readonlyUsersQuota = readonlyUsersQuota;
    }

    public long getReadonlyUsers() {
        return readonlyUsers;
    }

    public void setReadonlyUsers(long readonlyUsers) {
        this.readonlyUsers = readonlyUsers;
    }

    public long getApiCalls() {
        return apiCalls;
    }

    public void setApiCalls(long apiCalls) {
        this.apiCalls = apiCalls;
    }

    public Long getTicketsQuota() {
        return ticketsQuota;
    }

    public void setTicketsQuota(Long ticketsQuota) {
        this.ticketsQuota = ticketsQuota;
    }

    public long getTickets() {
        return tickets;
    }

    public void setTickets(long tickets) {
        this.tickets = tickets;
    }

    public Long getImportFoldersQuota() {
        return importFoldersQuota;
    }

    public void setImportFoldersQuota(Long importFoldersQuota) {
        this.importFoldersQuota = importFoldersQuota;
    }

    public Long getMaxEmailAccounts() {
        return maxEmailAccounts;
    }

    public void setMaxEmailAccounts(Long maxEmailAccounts) {
        this.maxEmailAccounts = maxEmailAccounts;
    }

    public long getEmailAccounts() {
        return emailAccounts;
    }

    public void setEmailAccounts(long emailAccounts) {
        this.emailAccounts = emailAccounts;
    }

    public long getPages() {
        return pages;
    }

    public void setPages(long pages) {
        this.pages = pages;
    }

    public Long getPagesQuota() {
        return pagesQuota;
    }

    public void setPagesQuota(Long pagesQuota) {
        this.pagesQuota = pagesQuota;
    }

    public long getStorage() {
        return storage;
    }

    public void setStorage(long storage) {
        this.storage = storage;
    }
}