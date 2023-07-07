package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Parameters to configure how contacts should be parsed
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class GUIParseContactsParameters implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * the separator char
	 */
	private String separator = ";";

	/**
	 * the fields delimiter
	 */
	private String delimiter = "\"";

	/**
	 * if the first row contains titles and must be skipped
	 */
	private boolean skipFirstRow = true;

	/**
	 * index of the the first name
	 */
	private int firstName = 0;

	/**
	 * firstName the last name
	 */
	private int lastName = 1;

	/**
	 * index of the email
	 */
	private int email = 2;

	/**
	 * index of the company
	 */
	private int company = 3;

	/**
	 * index of the phone
	 */
	private int phone = 4;

	/**
	 * index of the mobile phone
	 */
	private int mobile = 5;

	/**
	 * index of the address
	 */
	private int address = 6;

	public GUIParseContactsParameters() {
	}
	
	public GUIParseContactsParameters(String separator, String delimiter, boolean skipFirstRow) {
		super();
		this.separator = separator;
		this.delimiter = delimiter;
		this.skipFirstRow = skipFirstRow;
	}

	public String getSeparator() {
		return separator;
	}

	public void setSeparator(String separator) {
		this.separator = separator;
	}

	public String getDelimiter() {
		return delimiter;
	}

	public void setDelimiter(String delimiter) {
		this.delimiter = delimiter;
	}

	public boolean isSkipFirstRow() {
		return skipFirstRow;
	}

	public void setSkipFirstRow(boolean skipFirstRow) {
		this.skipFirstRow = skipFirstRow;
	}

	public int getFirstName() {
		return firstName;
	}

	public void setFirstName(int firstName) {
		this.firstName = firstName;
	}

	public int getLastName() {
		return lastName;
	}

	public void setLastName(int lastName) {
		this.lastName = lastName;
	}

	public int getEmail() {
		return email;
	}

	public void setEmail(int email) {
		this.email = email;
	}

	public int getCompany() {
		return company;
	}

	public void setCompany(int company) {
		this.company = company;
	}

	public int getPhone() {
		return phone;
	}

	public void setPhone(int phone) {
		this.phone = phone;
	}

	public int getMobile() {
		return mobile;
	}

	public void setMobile(int mobile) {
		this.mobile = mobile;
	}

	public int getAddress() {
		return address;
	}

	public void setAddress(int address) {
		this.address = address;
	}
}