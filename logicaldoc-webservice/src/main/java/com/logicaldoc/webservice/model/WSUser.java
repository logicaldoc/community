package com.logicaldoc.webservice.model;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.xml.bind.annotation.XmlType;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.WorkingTime;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.time.DateUtil;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service User. Useful class to create repository Users.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@XmlType(name = "WSUser")
public class WSUser {
	protected static Logger log = LoggerFactory.getLogger(WSUser.class);

	@WSDoc(documented = false)
	public static final int TYPE_DEFAULT = 0;

	@WSDoc(documented = false)
	public static final int TYPE_SYSTEM = 1;

	@WSDoc(documented = false)
	public static final int TYPE_GUEST = 2;

	@WSDoc(documented = false)
	public static final int SOURCE_DEFAULT = 0;

	@WSDoc(documented = false)
	public static final int SOURCE_LDAP = 1;

	@WSDoc(documented = false)
	public static final int SOURCE_ACTIVE_DIRECTORY = 2;

	@WSDoc(documented = false)
	public static final long USERID_ADMIN = 1;

	@WSDoc(description = "unique identifier")
	private long id;

	private String username = "";

	private String password = "";

	private String decodedPassword = "";

	private String passwordmd4 = "";

	@WSDoc(required = false)
	private String name = "";

	@WSDoc(required = false)
	private String firstName = "";

	@WSDoc(required = false)
	private String street = "";

	@WSDoc(required = false)
	private String postalcode = "";

	@WSDoc(required = false)
	private String city = "";

	@WSDoc(required = false)
	private String country = "";

	@WSDoc(required = false)
	private String state = "";

	@WSDoc(description = "default language; <a href='/wiki/LanguageSpecification'>See specification</a>")
	private String language = "";

	@WSDoc(required = true, description = "address used for notifications, must be a valid e-mail")
	private String email = "";

	@WSDoc(description = "a simple text to be used as a signature in the footer of the outgoing emails", required = false)
	private String emailSignature;

	@WSDoc(required = false, description = "secondary email address, must be a valid e-mail")
	private String email2 = "";

	@WSDoc(description = "a simple text to be used as a signature in the footer of the outgoing emails", required = false)
	private String emailSignature2;

	@WSDoc(required = false)
	private String telephone = "";

	@WSDoc(required = false)
	private String telephone2 = "";

	@WSDoc(description = "must be <b>0: normal or 2: guest</b>")
	private int type = TYPE_DEFAULT;

	@WSDoc(description = "ids of the groups this user belongs to")
	private long[] groupIds = new long[0];

	@WSDoc(description = "if <b>1</b> the user is enabled, if <b>0</b> the user is disabled")
	private int enabled = 1;

	@WSDoc(description = "if <b>1</b> the user can connect during working time only")
	private int enforceWorkingTime = 0;

	@WSDoc(description = "last time the password was changed (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private String passwordChanged = "";

	@WSDoc(description = "if <b>1</b> the password is eligible for expiration, if <b>0</b> the password never expires")
	private int passwordExpires = 0;

	@WSDoc(description = "must be <b>0</b>")
	private int source = 0;

	@WSDoc(description = "maximum allowed user's quota expressed in bytes, <b>-1</b> for no limits")
	private long quota = -1;

	@WSDoc(description = " actual quota used by the user ")
	private long quotaCount = 0;

	@WSDoc(required = false)
	private String lastModified;

	@WSDoc(required = false, description = "last time the user has logged in")
	private String lastLogin;

	@WSDoc(required = false, description = "when the user has been created")
	private String creation;

	@WSDoc(description = "date format to use when display dates")
	private String dateFormat;

	@WSDoc(description = "date format to use when display dates in short format")
	private String dateFormatShort;

	@WSDoc(description = "date format to use when display dates and times")
	private String dateFormatLong;

	@WSDoc(description = "comma separated list of searches that defines the order they are displayed in the user interface")
	private String searchPref;

	@WSDoc(required = false, description = "when this account expires")
	private String expire;

	@WSDoc(required = false, description = "the working time specification")
	private WSWorkingTime[] workingTimes = null;

	@WSDoc(required = false, description = "maximum number of inactivity days after which the account gets disabled")
	private Integer maxInactivity;

	@WSDoc(required = false, description = "the time zone of the suer")
	private String timeZone;

	@WSDoc(required = false, description = "the second factor authenticator to use")
	private String secondFactor;

	@WSDoc(required = false, description = "key used by the second factor authenticator")
	private String key;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
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

	public void setUsername(String uname) {
		username = uname;
	}

	/**
	 * Sets the password and encode it
	 * 
	 * @param passwd The password in readable format
	 */
	public void setPassword(String passwd) {
		decodedPassword = passwd;
		password = null;
		if (org.apache.commons.lang.StringUtils.isNotEmpty(passwd))
			password = CryptUtil.cryptString(passwd);
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

	public long[] getGroupIds() {
		return groupIds;
	}

	public void setGroupIds(long[] groupIds) {
		this.groupIds = groupIds;
	}

	public String toString() {
		return getUsername();
	}

	/**
	 * @return the name of the group associated to this user, that is
	 *         '_user_'+id
	 */
	public String getUserGroupName() {
		return "_user_" + getId();
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

	/**
	 * @return when the password was modified
	 */
	public String getPasswordChanged() {
		return passwordChanged;
	}

	public void setPasswordChanged(String passwordChanged) {
		this.passwordChanged = passwordChanged;
	}

	/**
	 * @return if the password expires or not
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
	 * @see User#SOURCE_DEFAULT
	 * @see User#SOURCE_LDAP
	 * 
	 * @return the source
	 */
	public int getSource() {
		return source;
	}

	public void setSource(int source) {
		this.source = source;
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

	public String getLastModified() {
		return lastModified;
	}

	public void setLastModified(String lastModified) {
		this.lastModified = lastModified;
	}

	public User toUser() {
		User user = new User();

		try {
			user.setId(getId());
			user.setCity(getCity());
			user.setCountry(getCountry());
			user.setEmail(getEmail());
			user.setEmail2(getEmail2());
			user.setFirstName(getFirstName());
			user.setName(getName());
			user.setLanguage(getLanguage());
			user.setPostalcode(getPostalcode());
			user.setState(getState());
			user.setStreet(getStreet());
			user.setTelephone(getTelephone());
			user.setTelephone2(getTelephone2());
			user.setUsername(getUsername());
			user.setEnabled(getEnabled());
			user.setPasswordExpires(getPasswordExpires());
			user.setQuota(getQuota());
			user.setType(getType());
			user.setSource(getSource());
			user.setPassword(getPassword());
			user.setPasswordChanged(new Date());
			user.setEmailSignature(getEmailSignature());
			user.setEmailSignature2(getEmailSignature2());
			user.setDateFormat(getDateFormat());
			user.setDateFormatShort(getDateFormatShort());
			user.setDateFormatLong(getDateFormatLong());
			user.setSearchPref(getSearchPref());
			user.setExpire(WSUtil.convertStringToDate(getExpire()));
			user.setEnforceWorkingTime(getEnforceWorkingTime());
			user.setMaxInactivity(getMaxInactivity());
			user.setTimeZone(getTimeZone());
			user.setKey(getKey());
			user.setSecondFactor(getSecondFactor());

			if (getGroupIds().length > 0) {
				GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
				Set<Group> groups = new HashSet<>();
				for (long groupId : getGroupIds()) {
					Group group = groupDao.findById(groupId);
					if (group != null)
						groups.add(group);
				}
				if (CollectionUtils.isNotEmpty(groups))
					user.setGroups(groups);
			}

			if (workingTimes != null && workingTimes.length > 0)
				for (WSWorkingTime wswt : workingTimes) {
					WorkingTime wt = new WorkingTime();
					BeanUtils.copyProperties(wt, wswt);
					user.getWorkingTimes().add(wt);
				}
		} catch (Exception e) {
			// Nothing to do
		}

		return user;
	}

	public static WSUser fromUser(User user) {
		if (user.getId() != 0L) {
			UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
			dao.initialize(user);
		}

		WSUser wsUser = new WSUser();
		try {
			wsUser.setId(user.getId());
			wsUser.setCity(user.getCity());
			wsUser.setCountry(user.getCountry());
			wsUser.setEmail(user.getEmail());
			wsUser.setEmail2(user.getEmail2());
			wsUser.setFirstName(user.getFirstName());
			wsUser.setName(user.getName());
			wsUser.setLanguage(user.getLanguage());
			wsUser.setPostalcode(user.getPostalcode());
			wsUser.setState(user.getState());
			wsUser.setStreet(user.getStreet());
			wsUser.setTelephone(user.getTelephone());
			wsUser.setTelephone2(user.getTelephone2());
			wsUser.setUsername(user.getUsername());
			wsUser.setEnabled(user.getEnabled());
			wsUser.setPasswordExpires(user.getPasswordExpires());
			wsUser.setQuota(user.getQuota());
			wsUser.setType(user.getType());
			wsUser.setSource(user.getSource());
			wsUser.setPassword(user.getPassword());
			wsUser.setPasswordmd4(user.getPasswordmd4());
			wsUser.setPasswordChanged(DateUtil.format(user.getPasswordChanged()));
			wsUser.setLastModified(DateUtil.format(user.getLastModified()));
			wsUser.setLastLogin(DateUtil.format(user.getLastLogin()));
			wsUser.setCreation(DateUtil.format(user.getCreation()));
			wsUser.setEmailSignature(user.getEmailSignature());
			wsUser.setEmailSignature2(user.getEmailSignature2());
			wsUser.setDateFormat(user.getDateFormat());
			wsUser.setDateFormatShort(user.getDateFormatShort());
			wsUser.setDateFormatLong(user.getDateFormatLong());
			wsUser.setSearchPref(user.getSearchPref());
			wsUser.setExpire(DateUtil.format(user.getExpire()));
			wsUser.setEnforceWorkingTime(user.getEnforceWorkingTime());
			wsUser.setMaxInactivity(user.getMaxInactivity());
			wsUser.setTimeZone(user.getTimeZone());
			wsUser.setKey(user.getKey());
			wsUser.setSecondFactor(user.getSecondFactor());

			if (CollectionUtils.isNotEmpty(user.getGroups())) {
				long[] groupIds = new long[user.getGroups().size()];
				int i = 0;
				for (Group group : user.getGroups()) {
					if (group.getType() == Group.TYPE_DEFAULT) {
						groupIds[i] = group.getId();
						i++;
					}
				}
				wsUser.setGroupIds(groupIds);
			}

			if (user.getWorkingTimes() != null && !user.getWorkingTimes().isEmpty()) {
				List<WSWorkingTime> tmp = user.getWorkingTimes().stream().map(wt -> {
					WSWorkingTime wswt = new WSWorkingTime();
					try {
						BeanUtils.copyProperties(wswt, wt);
					} catch (Exception t) {
						// Nothing to do
					}
					return wswt;
				}).collect(Collectors.toList());
				wsUser.setWorkingTimes(tmp.toArray(new WSWorkingTime[0]));
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return wsUser;
	}

	public String getPasswordmd4() {
		return passwordmd4;
	}

	public void setPasswordmd4(String passwordmd4) {
		this.passwordmd4 = passwordmd4;
	}

	public String getEmailSignature() {
		return emailSignature;
	}

	public void setEmailSignature(String emailSignature) {
		this.emailSignature = emailSignature;
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

	public String getExpire() {
		return expire;
	}

	public void setExpire(String expire) {
		this.expire = expire;
	}

	public int getEnforceWorkingTime() {
		return enforceWorkingTime;
	}

	public void setEnforceWorkingTime(int enforceWorkingTime) {
		this.enforceWorkingTime = enforceWorkingTime;
	}

	public WSWorkingTime[] getWorkingTimes() {
		return workingTimes;
	}

	public void setWorkingTimes(WSWorkingTime[] workingTimes) {
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

	public String getDecodedPassword() {
		return decodedPassword;
	}

	public void setDecodedPassword(String decodedPassword) {
		this.decodedPassword = decodedPassword;
	}

	public String getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(String lastLogin) {
		this.lastLogin = lastLogin;
	}

	public String getCreation() {
		return creation;
	}

	public void setCreation(String creation) {
		this.creation = creation;
	}
}