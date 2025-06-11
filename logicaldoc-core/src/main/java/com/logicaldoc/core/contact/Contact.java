package com.logicaldoc.core.contact;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;

/**
 * A generic contact represented by a set of personal informations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
@Entity
@Table(name = "ld_contact")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Contact extends PersistentObject {
	private static final long serialVersionUID = 1L;

	@Column(name = "ld_userid")
	private Long userId;

	@Column(name = "ld_firstname", length = 255)
	private String firstName;

	@Column(name = "ld_lastname", length = 255)
	private String lastName;

	@Column(name = "ld_company", length = 255)
	private String company;

	@Column(name = "ld_email", length = 512)
	private String email;

	@Column(name = "ld_phone", length = 255)
	private String phone;

	@Column(name = "ld_mobile", length = 255)
	private String mobile;

	@Column(name = "ld_address", length = 512)
	private String address;

	public Contact() {
	}

	public Contact(Contact source) {
		this.userId = source.userId;
		this.firstName = source.firstName;
		this.lastName = source.lastName;
		this.company = source.company;
		this.email = source.email;
		this.phone = source.phone;
		this.mobile = source.mobile;
		this.address = source.address;

		setTenantId(source.getTenantId());
	}

	public Long getUserId() {
		return userId;
	}

	public void setUserId(Long userId) {
		this.userId = userId;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getCompany() {
		return company;
	}

	public void setCompany(String company) {
		this.company = company;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
	}

	public String getFullName() {
		final String lstName = lastName != null ? " " + lastName : "";
		return firstName != null ? firstName : lstName;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	@Override
	public String toString() {
		return getFullName() + (email != null ? " - " + email : "");
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((email == null) ? 0 : email.hashCode());
		result = prime * result + ((userId == null) ? 0 : userId.hashCode());
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
		Contact other = (Contact) obj;
		if (email == null) {
			if (other.email != null)
				return false;
		} else if (!email.equals(other.email))
			return false;
		if (userId == null) {
			if (other.userId != null)
				return false;
		} else if (!userId.equals(other.userId))
			return false;
		return true;
	}
}