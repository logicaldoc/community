package com.logicaldoc.core.ticket;

import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.UUID;

import org.hsqldb.lib.StringUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.crypt.CryptUtil;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

/**
 * Represents ticket, most of the time this is used to model download tickets.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 */
@Entity
@Table(name = "ld_ticket")
@Cacheable
public class Ticket extends PersistentObject {

    private static final long serialVersionUID = 1L;

    private static Logger log = LoggerFactory.getLogger(Ticket.class);

    public static final int DOWNLOAD = 0;

    public static final int PSW_RECOVERY = 1;

    public static final int VIEW = 2;

    public static final int SUPPORT = 3;

    public static final int WHATSASAPP_AUTH = 4;

    @Column(name = "ld_ticketid", nullable = false)
    private String ticketId = UUID.randomUUID().toString();

    @Column(name = "ld_docid", nullable = false)
    private long docId = 0;

    @Column(name = "ld_suffix")
    private String suffix;

    @Column(name = "ld_userid", nullable = false)
    private long userId = -1;

    @Column(name = "ld_username", length = 255)
    private String username = "";

    @Column(name = "ld_userlogin", length = 255)
    private String userLogin = "";

    @Column(name = "ld_targetuserid")
    private Long targetUserId;

    @Column(name = "ld_targetusername", length = 255)
    private String targetUsername;

    @Column(name = "ld_targetlogin", length = 255)
    private String targetLogin;

    @Column(name = "ld_type", nullable = false)
    private int type = DOWNLOAD;

    /**
     * A date when this ticket expires
     */
    @Column(name = "ld_expired", columnDefinition = "DATETIME(3)")
    private Date expired = null;

    @Column(name = "ld_count", nullable = false)
    private int count = 0;

    /**
     * Maximum number of downloads
     */
    @Column(name = "ld_maxcount", nullable = true)
    private Integer maxCount;

    @Column(name = "ld_enabled", nullable = false)
    private boolean enabled = true;

    @Column(name = "ld_views", nullable = false)
    private int views = 0;

    /**
     * Maximum number of views
     */
    @Column(name = "ld_maxviews", nullable = true)
    private Integer maxViews;

    /**
     * Not persistent field
     */
    @Transient
    private String url;

    /**
     * Not persistent field
     */
    @Transient
    private Integer expireHours;

    @Column(name = "ld_password", length = 255)
    private String password = null;

    @Transient
    private String decodedPassword;

    public long getDocId() {
        return docId;
    }

    public void setDocId(long docId) {
        this.docId = docId;
    }

    /**
     * @return Returns the ticketId.
     */
    public String getTicketId() {
        return ticketId;
    }

    /**
     * @param ticketId The ticketId to set.
     */
    public void setTicketId(String ticketId) {
        this.ticketId = ticketId;
    }

    public long getUserId() {
        return userId;
    }

    public void setUserId(long userId) {
        this.userId = userId;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public Date getExpired() {
        return expired;
    }

    public void setExpired(Date expired) {
        this.expired = expired;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public boolean isTicketExpired() {
        return !enabled || (expired != null && new Date().getTime() > expired.getTime())
                || (maxCount != null && maxCount > 0 && count >= maxCount);
    }

    public boolean isTicketViewExpired() {
        return !enabled || (expired != null && new Date().getTime() > expired.getTime())
                || (maxViews != null && maxViews > 0 && views >= maxViews);
    }

    public boolean checkPassword(String password) {
        try {
            return CryptUtil.encryptSHA256(password).equals(getPassword());
        } catch (NoSuchAlgorithmException e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    public String getSuffix() {
        return suffix;
    }

    public void setSuffix(String suffix) {
        this.suffix = suffix;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Integer getMaxCount() {
        return maxCount;
    }

    public void setMaxCount(Integer maxCount) {
        this.maxCount = maxCount;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public int getViews() {
        return views;
    }

    public void setViews(int views) {
        this.views = views;
    }

    public Integer getMaxViews() {
        return maxViews;
    }

    public void setMaxViews(Integer maxViews) {
        this.maxViews = maxViews;
    }

    public Integer getExpireHours() {
        return expireHours;
    }

    public void setExpireHours(Integer expireHours) {
        this.expireHours = expireHours;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String pwd) {
        password = pwd;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getUserLogin() {
        return userLogin;
    }

    public void setUserLogin(String userLogin) {
        this.userLogin = userLogin;
    }

    public Long getTargetUserId() {
        return targetUserId;
    }

    public void setTargetUserId(Long targetUserId) {
        this.targetUserId = targetUserId;
    }

    public String getTargetUsername() {
        return targetUsername;
    }

    public void setTargetUsername(String targetUsername) {
        this.targetUsername = targetUsername;
    }

    public String getTargetLogin() {
        return targetLogin;
    }

    public void setTargetLogin(String targetLogin) {
        this.targetLogin = targetLogin;
    }

    /**
     * Sets the password and encode it
     * 
     * @param pwd The password in readable format
     * @throws NoSuchAlgorithmException Crypting error
     */
    public void setDecodedPassword(String pwd) throws NoSuchAlgorithmException {
        decodedPassword = pwd;
        if (StringUtil.isEmpty(pwd)) {
            password = null;
        } else {
            password = CryptUtil.encryptSHA256(pwd);
        }
    }

    public String getDecodedPassword() {
        return decodedPassword;
    }

    public void setUser(User user) {
        setTenantId(user.getTenantId());
        setUserId(user.getId());
        setUsername(user.getFullName());
        setUserLogin(user.getUsername());
    }

    public void setTargetUser(User user) {
        if (user != null) {
            setTargetUserId(user.getId());
            setTargetUsername(user.getFullName());
            setTargetLogin(user.getUsername());
        } else {
            setTargetUserId(null);
            setTargetUsername(null);
            setTargetLogin(null);
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((ticketId == null) ? 0 : ticketId.hashCode());
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
        Ticket other = (Ticket) obj;
        if (ticketId == null) {
            if (other.ticketId != null)
                return false;
        } else if (!ticketId.equals(other.ticketId))
            return false;
        return true;
    }
}