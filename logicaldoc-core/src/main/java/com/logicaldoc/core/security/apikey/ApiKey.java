package com.logicaldoc.core.security.apikey;

import java.security.NoSuchAlgorithmException;
import java.util.Date;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang.StringUtils;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.util.crypt.CryptUtil;

/**
 * A secret key to use when connecting to the Webservices API
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
@Entity
@Table(name = "ld_apikey")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class ApiKey extends PersistentObject {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_userid", nullable = false)
	private long userId;

	@Column(name = "ld_name", length = 255, nullable = false)
	private String name;

	@Column(name = "ld_lastused")
	private Date lastUsed;
	
	@Column(name = "ld_key", length = 255, nullable = false)
	private String key;
	
	@Column(name = "ld_label", length = 255, nullable = false)
	private String label;

	@Transient
	private String decodedKey;

	public ApiKey() {
		// Empty
	}

	public ApiKey(long userId, String name) {
		super();
		this.userId = userId;
		this.name = name;
	}

	public long getUserId() {
		return userId;
	}

	public String getName() {
		return name;
	}

	public Date getLastUsed() {
		return lastUsed;
	}

	public String getKey() {
		return key;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setLastUsed(Date lastUsed) {
		this.lastUsed = lastUsed;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public String getDecodedKey() {
		return decodedKey;
	}

	/**
	 * Sets the key and encode it
	 * 
	 * @param decodedKey The key in readable format
	 * @throws NoSuchAlgorithmException Cripting error
	 */
	public void setDecodedKey(String decodedKey) throws NoSuchAlgorithmException {
		if (StringUtils.isNotEmpty(decodedKey)) {
			this.decodedKey = decodedKey;
			this.key = CryptUtil.encryptSHA256(decodedKey);
			this.label = StringUtils.abbreviate(decodedKey, 10)
					+ (decodedKey.length() > 14 ? StringUtils.right(decodedKey, 4) : "");
		}
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	@Override
	public String toString() {
		return label;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((key == null) ? 0 : key.hashCode());
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
		ApiKey other = (ApiKey) obj;
		if (key == null) {
			if (other.key != null)
				return false;
		} else if (!key.equals(other.key))
			return false;
		return true;
	}
}