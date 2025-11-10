package com.logicaldoc.core.security.user;

import java.io.Serializable;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.spring.Context;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

/**
 * This class represents a user. A user can be member of any number of groups,
 * but it is always member of a special group named '_user_'+id. When a new user
 * is created this special group of type 'user' is also created.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 * @version 1.0
 */
@Entity
@Table(name = "ld_user")
@Cacheable
public class User extends PersistentObject implements Serializable {

	private static final Logger log = LoggerFactory.getLogger(User.class);

	public static final long USERID_ADMIN = 1;

	public static final long USERID_SYSTEM = -1010;

	private static final long serialVersionUID = 8093874904302301982L;
	
	@Column(name = "ld_type", nullable = false)
	@Enumerated(EnumType.ORDINAL)
	private UserType type = UserType.DEFAULT;

	@Column(name = "ld_source", nullable = false)
	@Enumerated(EnumType.ORDINAL)
	private UserSource source = UserSource.DEFAULT;

	@Column(name = "ld_username", length = 255, nullable = false)
	private String username = "";

	@Column(name = "ld_password", length = 255)
	private String password = "";

	@Column(name = "ld_passwordmd4", length = 255)
	private String passwordmd4 = "";

	@Column(name = "ld_name", length = 255)
	private String name = "";

	@Column(name = "ld_firstname", length = 255)
	private String firstName = "";

	@Column(name = "ld_street", length = 255)
	private String street = "";

	@Column(name = "ld_postalcode", length = 255)
	private String postalcode = "";

	@Column(name = "ld_city", length = 255)
	private String city = "";

	@Column(name = "ld_country", length = 255)
	private String country = "";

	@Column(name = "ld_state", length = 255)
	private String state = "";

	@Column(name = "ld_language", length = 10)
	private String language = "";

	@Column(name = "ld_email", length = 255)
	private String email = "";

	/**
	 * A simple text to be used as a signature in the footer of the outgoing
	 * emails
	 */
	@Column(name = "ld_emailsignature", length = 1000)
	private String emailSignature;

	@Column(name = "ld_email2", length = 255)
	private String email2 = "";

	@Column(name = "ld_emailsignature2", length = 1000)
	private String emailSignature2;

	@Column(name = "ld_telephone", length = 255)
	private String telephone = "";

	@Column(name = "ld_telephone2", length = 255)
	private String telephone2 = "";
	
	// Not persisted
	@Transient
	private Set<Group> groups = new HashSet<>();

	@Column(name = "ld_enabled", nullable = false)
	private int enabled = 1;

	/**
	 * If the users must confirm changes in the legals or not
	 */
	@Column(name = "ld_legals", nullable = false)
	private int legals = 0;
	
	/**
	 * The last time the password was changed
	 */
	@Column(name = "ld_passwordchanged", columnDefinition = "DATETIME(3)")
	private Date passwordChanged = new Date();

	/**
	 * If the password expires or not
	 */
	@Column(name = "ld_passwordexpires", nullable = false)
	private int passwordExpires = 0;

	/**
	 * If the password already expired
	 */
	@Column(name = "ld_passwordexpired", nullable = false)
	private int passwordExpired = 0;

	/**
	 * Only for GUI
	 */
	@Transient
	private String repass;

	@Column(name = "ld_quota", nullable = false)
	private long quota = -1;

	@Column(name = "ld_welcomescreen", nullable = true)
	private Integer welcomeScreen = 1520;

	@Column(name = "ld_defworkspace")
	private Long defaultWorkspace;

	@Column(name = "ld_ipwhitelist", length = 1000, nullable = true)
	private String ipWhiteList;

	@Column(name = "ld_ipblacklist", length = 1000, nullable = true)
	private String ipBlackList;

	/**
	 * When the certificate expires
	 */
	@Column(name = "ld_certexpire", columnDefinition = "DATETIME(3)")
	private Date certExpire;

	/**
	 * The distinguished name of the certificate
	 */
	@Column(name = "ld_certdn", length = 1000)
	private String certDN;

	/**
	 * The second factor authenticator to use
	 */
	@Column(name = "ld_secondfactor", length = 255)
	private String secondFactor;

	/**
	 * A key used by the second factor authenticator
	 */
	@Column(name = "ld_key", length = 255)
	private String key;

	/**
	 * Description of the grid that displays the list of documents
	 */
	@Column(name = "ld_docsgrid", nullable = true)
	private String docsGrid;

	/**
	 * Description of the grid that shows the results of a search
	 */
	@Column(name = "ld_hitsgrid", nullable = true)
	private String hitsGrid;

	@Column(name = "ld_dateformat", length = 255)
	private String dateFormat;

	@Column(name = "ld_dateformatshort", length = 255)
	private String dateFormatShort;

	@Column(name = "ld_dateformatlong", length = 255)
	private String dateFormatLong;

	/**
	 * Comma separated list of searches that defines the order they are
	 * displayed in the user interface
	 */
	@Column(name = "ld_searchpref", length = 255)
	private String searchPref;

	/**
	 * Last time this account has been activated
	 */
	@Column(name = "ld_lastenabled")
	private Date lastEnabled;

	/**
	 * When this account expires
	 */
	@Column(name = "ld_expire", columnDefinition = "DATETIME(3)")
	private Date expire;

	/**
	 * If the system must forbid the login outside the working time
	 */
	@Column(name = "ld_enforcewrktime", nullable = false)
	private int enforceWorkingTime = 0;

	/**
	 * Maximum number of inactivity days after which the account gets disabled.
	 * Possible values are:
	 * <ul>
	 * <li>null: the general setting is used instead</li>
	 * <li>&lt;=0: no checks will be done for this user</li>
	 * <li>&gt;0: number of maximum inactivity days</li>
	 * </ul>
	 */
	@Column(name = "ld_maxinactivity")
	private Integer maxInactivity;

	@Column(name = "ld_timezone")
	private String timeZone;

	/**
	 * Base64 representation of the avatar image
	 */
	@Column(name = "ld_avatar")
	private String avatar;

	/**
	 * Last time the user successfully logged in
	 */
	@Column(name = "ld_lastlogin", columnDefinition = "DATETIME(3)")
	private Date lastLogin = new Date();

	@Transient
	private String decodedPassword;

	@Transient
	private Set<UserGroup> userGroups = new HashSet<>();

	/**
	 * Collection of all the admitted working times
	 */
	@Transient
	private Set<WorkingTime> workingTimes = new HashSet<>();

	@Column(name = "ld_department", length = 255)
	private String department;

	@Column(name = "ld_organizationalunit", length = 255)
	private String organizationalUnit;

	@Column(name = "ld_building", length = 255)
	private String building;

	@Column(name = "ld_company", length = 255)
	private String company;

	/**
	 * If the user wants to enable the evaluation form
	 */
	@Column(name = "ld_evalform", nullable = false)
	private int evalFormEnabled = 1;

	public int getEvalFormEnabled() {
		return evalFormEnabled;
	}

	public void setEvalFormEnabled(int evalFormEnabled) {
		this.evalFormEnabled = evalFormEnabled;
	}

	public String getCompany() {
		return company;
	}

	public void setCompany(String company) {
		this.company = company;
	}

	public UserType getType() {
		return type;
	}

	public void setType(UserType type) {
		this.type = type;
	}
	
	public void setType(int type) {
		this.type = UserType.values()[type];
	}

	public String getRepass() {
		return repass;
	}

	public void setRepass(String repass) {
		this.repass = repass;
	}

	public String getUsername() {
		return username;
	}

	public String getPassword() {
		return password;
	}

	public String getName() {
		return name;
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

	public String getInitials() {
		StringBuilder sb = new StringBuilder();
		String fullName = getFullName();
		String[] tokens = fullName.split(" ");
		for (String token : tokens) {
			if (!token.trim().isEmpty())
				sb.append(token.trim().toUpperCase().substring(0, 1));
		}
		return sb.toString();
	}

	public String getFirstName() {
		return firstName;
	}

	public String getStreet() {
		return street;
	}

	public String getPostalcode() {
		return postalcode;
	}

	public String getCity() {
		return city;
	}

	public String getCountry() {
		return country;
	}

	public String getLanguage() {
		return language;
	}

	public String getEmail() {
		return email;
	}

	public String getTelephone() {
		return telephone;
	}

	/**
	 * Checks if the user is member of the admin group
	 * 
	 * @return true only if the user belongs to the admin group
	 */
	public boolean isAdmin() {
		return isMemberOf("admin");
	}

	public boolean isMemberOf(String groupName) {
		return groups.stream().anyMatch(g -> groupName.equals(g.getName()));
	}

	public void addGroup(Group group) {
		if (group == null)
			return;

		if (!getGroups().contains(group))
			getGroups().add(group);

		UserGroup ug = new UserGroup(group.getId());
		if (!getUserGroups().contains(ug))
			getUserGroups().add(ug);
	}

	public void removeGroup(long groupId) {
		Iterator<UserGroup> iter = getUserGroups().iterator();
		while (iter.hasNext()) {
			UserGroup p = iter.next();
			if (p.getGroupId() == groupId)
				iter.remove();
		}

		Iterator<Group> iter2 = getGroups().iterator();
		while (iter2.hasNext()) {
			Group p = iter2.next();
			if (p.getId() == groupId)
				iter2.remove();
		}
	}

	/**
	 * Removes the user from All groups except it's user's own group. You can
	 * restrict the deletion to a specified source
	 * 
	 * @param source the source
	 */
	public void removeGroupMemberships(String source) {
		Iterator<Group> iter = getGroups().iterator();
		while (iter.hasNext()) {
			Group grp = iter.next();
			if (!getUserGroupName().equals(grp.getName()) && (source == null || source.equals(grp.getSource()))) {
				iter.remove();
				getUserGroups().remove(new UserGroup(grp.getId()));
			}
		}
	}

	public void setUsername(String uname) {
		username = uname;
	}

	public void setPassword(String pwd) {
		password = pwd;
	}

	/**
	 * Sets the password and encode it
	 * 
	 * @param pwd The password in readable format
	 * @throws NoSuchAlgorithmException Cripting error
	 */
	public void setDecodedPassword(String pwd) throws NoSuchAlgorithmException {
		if (StringUtils.isEmpty(pwd))
			throw new NoSuchAlgorithmException("Password cannot be empty");
		decodedPassword = pwd;
		password = CryptUtil.encryptSHA256(pwd);
		if (StringUtils.isEmpty(password))
			throw new NoSuchAlgorithmException("Password cannot be empty");
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public void setStreet(String str) {
		street = str;
	}

	public void setPostalcode(String pc) {
		postalcode = pc;
	}

	public void setCity(String ct) {
		city = ct;
	}

	public void setCountry(String cnt) {
		country = cnt;
	}

	public void setLanguage(String lang) {
		language = lang;
	}

	public void setEmail(String mail) {
		email = mail;
	}

	public void setTelephone(String phone) {
		telephone = phone;
	}

	public void reset() {
		username = "";
		password = "";
		passwordmd4 = "";
		name = "";
		firstName = "";
		street = "";
		postalcode = "";
		city = "";
		country = "";
		language = "";
		email = "";
		telephone = "";
		docsGrid = null;
		hitsGrid = null;
		groups = new HashSet<>();
		passwordExpires = 0;
		avatar = null;
		expire = null;
		enforceWorkingTime = 0;
		lastLogin = null;
		workingTimes = new HashSet<>();
		building = null;
		organizationalUnit = null;
		department = null;
	}

	@Override
	public String toString() {
		return getUsername();
	}

	public Set<Group> getGroups() {
		return groups;
	}

	public void setGroups(Set<Group> groups) {
		this.groups = groups;
	}

	/**
	 * The name of the group associated to this user, that is '_user_'+id
	 * 
	 * @return name of the group that represents this user
	 */
	public String getUserGroupName() {
		return "_user_" + getId();
	}

	/**
	 * Retrieves this user's group
	 * 
	 * @return the group
	 * 
	 * @throws PersistenceException Error in the data layer
	 */
	public Group getUserGroup() throws PersistenceException {
		if (getGroups() != null)
			for (Group grp : getGroups()) {
				if (grp.getName().equals(getUserGroupName()))
					return grp;
			}

		// The special group was not found in the belonging groups, this may
		// indicate a lost association between this user and his group
		log.warn("User {} has lost association with his group", username);
		GroupDAO dao = Context.get(GroupDAO.class);
		Group group = dao.findByName(getUserGroupName(), getTenantId());
		if (group == null)
			log.warn("User {} doesn't have his user group {}", username, getUserGroupName());
		return group;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getTelephone2() {
		return telephone2;
	}

	public void setTelephone2(String telephone2) {
		this.telephone2 = telephone2;
	}

	public Locale getLocale() {
		return LocaleUtil.toLocale(getLanguage());
	}

	public void setLocale(Locale locale) {
		setLanguage(locale.toString());
	}

	/**
	 * When the password was modified
	 * 
	 * @return when the password has been altered
	 */
	public Date getPasswordChanged() {
		return passwordChanged;
	}

	public void setPasswordChanged(Date passwordChanged) {
		this.passwordChanged = passwordChanged;
	}

	/**
	 * If the password expires or not
	 * 
	 * @return <b>1</b> if the password expires, <b>0</b> otherwise
	 */
	public int getPasswordExpires() {
		return passwordExpires;
	}

	public void setPasswordExpires(int passwordExpires) {
		this.passwordExpires = passwordExpires;
	}

	/**
	 * The source from which the user has been created
	 * 
	 * @see UserSource
	 * 
	 * @return the source
	 */
	public UserSource getSource() {
		return source;
	}

	public void setSource(UserSource source) {
		this.source = source;
	}
	
	public void setSource(int source) {
		this.source = UserSource.values()[source];
	}

	public long getQuota() {
		return quota;
	}

	public void setQuota(long quota) {
		this.quota = quota;
	}

	public Integer getWelcomeScreen() {
		return welcomeScreen;
	}

	public void setWelcomeScreen(Integer welcomeScreen) {
		this.welcomeScreen = welcomeScreen;
	}

	public String getIpWhiteList() {
		return ipWhiteList;
	}

	public void setIpWhiteList(String ipWhiteList) {
		this.ipWhiteList = ipWhiteList;
	}

	public String getIpBlackList() {
		return ipBlackList;
	}

	public void setIpBlackList(String ipBlackList) {
		this.ipBlackList = ipBlackList;
	}

	public int getPasswordExpired() {
		return passwordExpired;
	}

	public void setPasswordExpired(int passwordExpired) {
		this.passwordExpired = passwordExpired;
	}

	public String getPasswordmd4() {
		return passwordmd4;
	}

	public void setPasswordmd4(String passwordmd4) {
		this.passwordmd4 = passwordmd4;
	}

	public String getDecodedPassword() {
		return decodedPassword;
	}

	public String getEmailSignature() {
		return emailSignature;
	}

	public void setEmailSignature(String emailSignature) {
		this.emailSignature = emailSignature;
	}

	public void clearPassword() {

	}

	public Long getDefaultWorkspace() {
		return defaultWorkspace;
	}

	public void setDefaultWorkspace(Long defaultWorkspace) {
		this.defaultWorkspace = defaultWorkspace;
	}

	public String getEmail2() {
		return email2;
	}

	public void setEmail2(String email2) {
		this.email2 = email2;
	}

	public String getEmailSignature2() {
		return emailSignature2;
	}

	public void setEmailSignature2(String emailSignature2) {
		this.emailSignature2 = emailSignature2;
	}

	public Set<UserGroup> getUserGroups() {
		return userGroups;
	}

	public void setUserGroups(Set<UserGroup> userGroups) {
		this.userGroups = userGroups;
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

	public String getSecondFactor() {
		return secondFactor;
	}

	public void setSecondFactor(String secondFactor) {
		this.secondFactor = secondFactor;
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public boolean isReadonly() {
		return type == UserType.READONLY;
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

	public String getAvatar() {
		return avatar;
	}

	public void setAvatar(String avatar) {
		this.avatar = avatar;
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

	public Date getExpire() {
		return expire;
	}

	public void setExpire(Date expire) {
		this.expire = expire;
	}

	public Date getLastEnabled() {
		return lastEnabled;
	}

	public void setLastEnabled(Date lastEnabled) {
		this.lastEnabled = lastEnabled;
	}

	/**
	 * Checks if the user is expired now
	 * 
	 * @return true only if an expiration date has been specified and the
	 *         current date is after it
	 */
	public boolean isExpired() {
		return getExpire() != null && getExpire().before(new Date());
	}

	/**
	 * Checks if the user falls in his working time
	 * 
	 * @return true only if at least a working time slot matches the current
	 *         time
	 */
	public boolean isInWorkingTime() {
		if (getWorkingTimes() != null)
			for (WorkingTime wt : getWorkingTimes()) {
				if (wt.matchesCurrentTime())
					return true;
			}
		return false;
	}

	public int getEnforceWorkingTime() {
		return enforceWorkingTime;
	}

	public void setEnforceWorkingTime(int enforceWorkingTime) {
		this.enforceWorkingTime = enforceWorkingTime;
	}

	public Set<WorkingTime> getWorkingTimes() {
		return workingTimes;
	}

	public void setWorkingTimes(Set<WorkingTime> workingTimes) {
		this.workingTimes = workingTimes;
	}

	public Integer getMaxInactivity() {
		return maxInactivity;
	}

	public void setMaxInactivity(Integer maxInactivity) {
		this.maxInactivity = maxInactivity;
	}

	public String getTimeZone() {
		return timeZone;
	}

	public void setTimeZone(String timeZone) {
		this.timeZone = timeZone;
	}

	public Date getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(Date lastLogin) {
		this.lastLogin = lastLogin;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getOrganizationalUnit() {
		return organizationalUnit;
	}

	public void setOrganizationalUnit(String organizationalUnit) {
		this.organizationalUnit = organizationalUnit;
	}

	public String getBuilding() {
		return building;
	}

	public void setBuilding(String building) {
		this.building = building;
	}

	public int getLegals() {
		return legals;
	}

	public void setLegals(int legals) {
		this.legals = legals;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((username == null) ? 0 : username.hashCode());
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
		User other = (User) obj;
		if (username == null) {
			if (other.username != null)
				return false;
		} else if (!username.equals(other.username))
			return false;
		return true;
	}
}