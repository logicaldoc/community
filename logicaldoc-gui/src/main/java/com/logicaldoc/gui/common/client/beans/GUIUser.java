package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;

/**
 * User bean as used in the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIUser implements Serializable {

	public static final int TYPE_DEFAULT = 0;

	public static final int TYPE_READONLY = 2;

	public static final String ALL_TASKS = "tasks";

	public static final String EVENTS = "events";

	public static final String UNREAD_MESSAGES = "unreadMessages";

	public static final String ALL_MESSAGES = "messages";

	public static final String LOCKED_DOCS = "lockedDocs";

	public static final String CHECKED_OUT_DOCS = "checkedOutDocs";

	public static final String ALL_SUBSCRIPTIONS = "subscriptions";

	private static final long serialVersionUID = 1L;

	private String username = "";

	private long id = 0;

	private GUIGroup[] groups = new GUIGroup[0];

	private String firstName = "";

	private String name = "";

	private String language = "en";

	private boolean notifyCredentials = true;

	private int passwordMinLenght = 0;

	private boolean passwordExpires = false;

	private boolean passwordExpired = false;

	private String address = "";

	private String postalCode = "";

	private String city = "";

	private String country = "";

	private String state = "";

	private String phone = "";

	private String email = "";

	private String emailSignature = null;

	private String email2 = "";

	private String emailSignature2 = null;

	private String cell = "";

	private boolean enabled = true;

	private int checkedOutDocs = 0;

	private int lockedDocs = 0;

	private int unreadMessages = 0;

	private int tasks = 0;

	private int messages = 0;

	private int subscriptions = 0;

	private int upcomingEvents = 0;

	private GUITenant tenant = null;

	private Long[] menus = new Long[0];

	private long quota = -1;

	private long quotaCount = 0;

	private Integer welcomeScreen = 1500;

	private String ipWhitelist;

	private String ipBlacklist;

	private GUIDashlet[] dashlets = new GUIDashlet[0];

	private GUIMenu[] customActions = new GUIMenu[0];

	private Long defaultWorkspace;

	private Date certExpire = null;

	private String certDN = null;

	private String secondFactor = null;

	private String key = null;

	private int type = 0;

	/**
	 * Description of the grid that displays the list of documents, a JSON
	 * format
	 */
	private String docsGrid;

	/**
	 * Description of the grid that shows the results of a search, a JSON format
	 */
	private String hitsGrid;

	private String dateFormat;

	private String dateFormatShort;

	private String dateFormatLong;

	private String searchPref;

	private Date expire;

	/**
	 * If the system must forbid the login outside the working time
	 */
	private boolean enforceWorkingTime = false;

	private GUIWorkingTime[] workingTimes = new GUIWorkingTime[0];

	private Integer maxInactivity;

	private String timeZone;

	/**
	 * The reason why the last login failed
	 */
	private String lastLoginFailureReason;

	private int source = 0;
	
	private Date creation;
	
	private Date lastLogin;

	public GUIUser() {
		tenant = new GUITenant();
		tenant.setId(Constants.TENANT_DEFAULTID);
		tenant.setName(Constants.TENANT_DEFAULTNAME);
		tenant.setDisplayName(Constants.TENANT_DEFAULTDISPLAYNAME);
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public String getSecondFactor() {
		return secondFactor;
	}

	public void setSecondFactor(String secondFactor) {
		this.secondFactor = secondFactor;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getUsername() {
		return username;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public GUIGroup[] getGroups() {
		return groups;
	}

	public void setGroups(GUIGroup[] groups) {
		this.groups = groups;
	}

	public boolean isMemberOf(String group) {
		for (GUIGroup g : groups) {
			if (group.equals(g.getName()))
				return true;
		}
		return false;
	}

	public String getFullName() {
		String fullName = getFirstName();
		if (fullName != null && getName() != null)
			fullName += " " + getName();
		if (fullName == null && getName() != null)
			fullName = getName();
		if (fullName == null)
			fullName = getUsername();
		return fullName;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getPasswordMinLenght() {
		return passwordMinLenght;
	}

	public void setPasswordMinLenght(int passwordMinLenght) {
		this.passwordMinLenght = passwordMinLenght;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public boolean isPasswordExpires() {
		return passwordExpires;
	}

	public void setPasswordExpires(boolean passwordExpires) {
		this.passwordExpires = passwordExpires;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
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

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getCell() {
		return cell;
	}

	public void setCell(String cell) {
		this.cell = cell;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public void addGroup(GUIGroup group) {
		if (!isMemberOf(group.getName())) {
			GUIGroup[] newGroups = Arrays.copyOf(groups, groups.length + 1);
			newGroups[groups.length] = group;
			groups = newGroups;
		}
	}

	public void addDashlet(GUIDashlet dashlet) {
		GUIDashlet[] newDashlets = Arrays.copyOf(dashlets, dashlets.length + 1);
		newDashlets[groups.length] = dashlet;
		dashlets = newDashlets;
	}

	public void removeGroup(String groupName) {
		if (groups.length == 0)
			return;

		if (isMemberOf(groupName)) {
			GUIGroup[] tmp = new GUIGroup[groups.length - 1];
			int i = 0;
			for (GUIGroup g : groups) {
				if (!g.getName().equals(groupName))
					tmp[i++] = g;
			}
			groups = tmp;
		}
	}

	public void removeDashlet(int id) {
		if (dashlets.length == 0)
			return;

		GUIDashlet[] tmp = new GUIDashlet[dashlets.length - 1];
		int i = 0;
		for (GUIDashlet g : dashlets) {
			if (g.getId() != id)
				tmp[i++] = g;
		}
		dashlets = tmp;

	}

	public int getCheckedOutDocs() {
		return checkedOutDocs;
	}

	public void setCheckedOutDocs(int checkedOutDocs) {
		if (this.checkedOutDocs != checkedOutDocs) {
			this.checkedOutDocs = checkedOutDocs;
		}
	}

	public int getLockedDocs() {
		return lockedDocs;
	}

	public void setLockedDocs(int lockedDocs) {
		if (this.lockedDocs != lockedDocs) {
			this.lockedDocs = lockedDocs;
		}
	}

	public int getMessages() {
		return messages;
	}

	public void setMessages(int messages) {
		if (this.messages != messages) {
			this.messages = messages;
		}
	}

	public int getUnreadMessages() {
		return unreadMessages;
	}

	public void setUnreadMessages(int unreadMessages) {
		if (this.unreadMessages != unreadMessages) {
			this.unreadMessages = unreadMessages;
		}
	}

	public int getAssignedTasks() {
		return tasks;
	}

	public void setAssignedTasks(int tasks) {
		this.tasks = tasks;
	}

	public Long[] getMenus() {
		return menus;
	}

	public void setMenus(Long[] menus) {
		this.menus = menus;
	}

	public int getSubscriptions() {
		return subscriptions;
	}

	public void setSubscriptions(int subscriptions) {
		if (this.subscriptions != subscriptions) {
			this.subscriptions = subscriptions;
		}
	}

	public long getQuota() {
		return quota;
	}

	public void setQuota(long quota) {
		this.quota = quota;
	}

	public long getQuotaCount() {
		return quotaCount;
	}

	public void setQuotaCount(long quotaCount) {
		this.quotaCount = quotaCount;
	}

	public Integer getWelcomeScreen() {
		return welcomeScreen;
	}

	public void setWelcomeScreen(Integer welcomeScreen) {
		this.welcomeScreen = welcomeScreen;
	}

	public String getIpWhitelist() {
		return ipWhitelist;
	}

	public void setIpWhitelist(String ipWhitelist) {
		this.ipWhitelist = ipWhitelist;
	}

	public String getIpBlacklist() {
		return ipBlacklist;
	}

	public void setIpBlacklist(String ipBlacklist) {
		this.ipBlacklist = ipBlacklist;
	}

	public GUIDashlet[] getDashlets() {
		return dashlets;
	}

	public void setDashlets(GUIDashlet[] dashlets) {
		this.dashlets = dashlets;
	}

	public boolean isNotifyCredentials() {
		return notifyCredentials;
	}

	public void setNotifyCredentials(boolean notifyCredentials) {
		this.notifyCredentials = notifyCredentials;
	}

	public boolean isPasswordExpired() {
		return passwordExpired;
	}

	public void setPasswordExpired(boolean passwordExpired) {
		this.passwordExpired = passwordExpired;
	}

	public int getUpcomingEvents() {
		return upcomingEvents;
	}

	public void setUpcomingEvents(int events) {
		if (this.upcomingEvents != events) {
			this.upcomingEvents = events;
		}
	}

	@Override
	public String toString() {
		return (getFirstName() != null ? getFirstName() : "") + " " + (getName() != null ? getName() : "");
	}

	public String getEmailSignature() {
		return emailSignature;
	}

	public String getEmailSignatureStr() {
		if (getEmailSignature() != null)
			return getEmailSignature();
		else
			return "";
	}

	public void setEmailSignature(String emailSignature) {
		this.emailSignature = emailSignature;
	}

	public Long getDefaultWorkspace() {
		return defaultWorkspace;
	}

	public void setDefaultWorkspace(Long defaultWorkspace) {
		this.defaultWorkspace = defaultWorkspace;
	}

	public String getEmailSignature2() {
		return emailSignature2;
	}

	public void setEmailSignature2(String emailSignature2) {
		this.emailSignature2 = emailSignature2;
	}

	public String getEmail2() {
		return email2;
	}

	public void setEmail2(String email2) {
		this.email2 = email2;
	}

	public Date getCertExpire() {
		return certExpire;
	}

	public void setCertExpire(Date certExpire) {
		this.certExpire = certExpire;
	}

	public String getCertDN() {
		return certDN;
	}

	public void setCertDN(String certDN) {
		this.certDN = certDN;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public boolean isReadOnly() {
		return type == TYPE_READONLY;
	}

	public GUITenant getTenant() {
		return tenant;
	}

	public void setTenant(GUITenant tenant) {
		this.tenant = tenant;
	}

	public String getDocsGrid() {
		return docsGrid;
	}

	public void setDocsGrid(String docsGrid) {
		this.docsGrid = docsGrid;
	}

	public String getHitsGrid() {
		return hitsGrid;
	}

	public void setHitsGrid(String hitsGrid) {
		this.hitsGrid = hitsGrid;
	}

	public GUIMenu[] getCustomActions() {
		return customActions;
	}

	public void setCustomActions(GUIMenu[] customActions) {
		this.customActions = customActions;
	}

	public void updateCustomAction(GUIMenu action) {
		if (customActions != null)
			for (int i = 0; i < customActions.length; i++) {
				if (customActions[i].getId() == action.getId())
					customActions[i] = action;
			}
	}

	public String getDateFormat() {
		return dateFormat;
	}

	public void setDateFormat(String dateFormat) {
		this.dateFormat = dateFormat;
	}

	public String getDateFormatShort() {
		return dateFormatShort;
	}

	public void setDateFormatShort(String dateFormatShort) {
		this.dateFormatShort = dateFormatShort;
	}

	public String getDateFormatLong() {
		return dateFormatLong;
	}

	public void setDateFormatLong(String dateFormatLong) {
		this.dateFormatLong = dateFormatLong;
	}

	public String getSearchPref() {
		return searchPref;
	}

	public void setSearchPref(String searchPref) {
		this.searchPref = searchPref;
	}

	public List<String> orderedSearches() {
		List<String> searches;
		if (searchPref != null && !searchPref.isEmpty()) {
			searches = Arrays.asList(searchPref.replace(" ", "").split(","));
		} else
			searches = Arrays.asList("fulltext", "parameters", "tags", "folders");
		return searches;
	}

	public Date getExpire() {
		return expire;
	}

	public void setExpire(Date expire) {
		this.expire = expire;
	}

	public boolean isEnforceWorkingTime() {
		return enforceWorkingTime;
	}

	public void setEnforceWorkingTime(boolean enforceWorkingTime) {
		this.enforceWorkingTime = enforceWorkingTime;
	}

	public GUIWorkingTime[] getWorkingTimes() {
		return workingTimes;
	}

	public void setWorkingTimes(GUIWorkingTime[] workingTimes) {
		this.workingTimes = workingTimes;
	}

	public Integer getMaxInactivity() {
		return maxInactivity;
	}

	public void setMaxInactivity(Integer maxInactivity) {
		this.maxInactivity = maxInactivity;
	}

	public String getLastLoginFailureReason() {
		return lastLoginFailureReason;
	}

	public void setLastLoginFailureReason(String lastLoginFailureReason) {
		this.lastLoginFailureReason = lastLoginFailureReason;
	}

	public String getTimeZone() {
		return timeZone;
	}

	public void setTimeZone(String timeZone) {
		this.timeZone = timeZone;
	}

	public int getSource() {
		return source;
	}

	public void setSource(int source) {
		this.source = source;
	}

	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	public Date getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(Date lastLogin) {
		this.lastLogin = lastLogin;
	}
}