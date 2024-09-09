package com.logicaldoc.core.communication;

import java.io.Serializable;

import org.apache.commons.lang3.StringUtils;

/**
 * A generic recipient of a message or email
 * 
 * @author Michael Scholz
 */
public class Recipient implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final int TYPE_SYSTEM = 0;

	public static final int TYPE_EMAIL = 1;

	public static final String MODE_EMAIL_TO = "TO";

	public static final String MODE_EMAIL_CC = "CC";

	public static final String MODE_EMAIL_BCC = "BCC";

	public static final String MODE_EMAIL_REPLYTO = "REPLYTO";

	// The login
	private String name = "";

	// The system login or the email address
	private String address = "";

	// The recipient mode (for the system message is not useful, for the email
	// can be To, CC, CCN, ecc.)
	private String mode = MODE_EMAIL_TO;

	// The recipient type (i.e. system, user, group, email)
	private int type = TYPE_SYSTEM;

	private int read = 0;

	public Recipient(String name, String address) {
		super();
		this.name = name;
		this.address = StringUtils.trimToEmpty(address);
	}

	public Recipient() {
	}

	public Recipient(Recipient source) {
		super();
		this.name = source.name;
		this.address = source.address;
		this.mode = source.mode;
		this.type = source.type;
		this.read = source.read;
	}

	public String getName() {
		return name;
	}

	public String getAddress() {
		return address;
	}

	public void setName(String nme) {
		name = nme;
	}

	public void setAddress(String addr) {
		address = StringUtils.trimToEmpty(addr);
	}

	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof Recipient))
			return false;
		Recipient other = (Recipient) arg0;
		return other.getAddress().equals(address);
	}

	@Override
	public int hashCode() {
		return  address.hashCode();
	}

	public String getMode() {
		return mode;
	}

	public void setMode(String mode) {
		this.mode = mode;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public int getRead() {
		return read;
	}

	public void setRead(int read) {
		this.read = read;
	}

	@Override
	public String toString() {
		return address;
	}
}