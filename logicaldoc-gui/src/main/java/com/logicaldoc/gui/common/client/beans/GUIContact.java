package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * GUI representation of a generic contact
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIContact implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private Long userId;

	private String firstName;

	private String lastName;

	private String company;

	private String email;

	private String phone;

	private String mobile;

	private String address;

	public GUIContact() {
	}

	public GUIContact(String email) {
		super();
		this.email = email;
	}

	public GUIContact(String firstName, String lastName, String email) {
		super();
		this.firstName = firstName;
		this.lastName = lastName;
		this.email = email;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public Long getUserId() {
		return userId;
	}

	public void setUserId(Long userId) {
		this.userId = userId;
	}

	public String getFullName() {
		String name = "";
		if (firstName != null)
			name += firstName.trim();
		if (lastName != null) {
			if (!name.isEmpty())
				name += " ";
			name += lastName;
		}
		return name;
	}

	public String displayLink() {
		String mailLink = "<a href='mailto:" + getEmail() + "'>" + getEmail() + "</a>";
		String name = getFullName();
		return name.isEmpty() || name.equals(email) ? mailLink : name + " &lt;" + mailLink + "&gt;";
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

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String displayString() {
		String display = "";
		if (firstName != null && !firstName.isEmpty())
			display += firstName;
		if (lastName != null && !lastName.isEmpty())
			display += " " + lastName;
		if (email != null && !email.isEmpty())
			display += " <" + email + ">";
		return display;
	}
}