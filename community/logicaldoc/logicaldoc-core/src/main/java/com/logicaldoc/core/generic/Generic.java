package com.logicaldoc.core.generic;

import java.util.Date;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.metadata.ExtensibleObject;

/**
 * Instances of this class represents generic informations in the database. Use
 * this Business entity to store configurations or stuffs like this.
 * <br>
 * 
 * Each Generic is identified by a type and subtype
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class Generic extends ExtensibleObject implements Comparable<Generic> {
	private String type;

	private String subtype;

	private Long qualifier;

	private String string1;

	private String string2;

	private String string3;

	private String string4;

	private Long integer1;

	private Long integer2;

	private Long integer3;

	private Double double1;

	private Double double2;

	private Date date1;

	private Date date2;

	public Generic() {
		super();
	}

	public Generic(String type, String subtype) {
		super();
		this.type = type;
		this.subtype = subtype;
	}

	public Generic(String type, String subtype, Long qualifier) {
		super();
		this.type = type;
		this.subtype = subtype;
		this.qualifier = qualifier;
	}

	public Generic(String type, String subtype, Long qualifier, long tenantId) {
		super();
		this.type = type;
		this.subtype = subtype;
		this.qualifier = qualifier;
		setTenantId(tenantId);
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getSubtype() {
		return subtype;
	}

	public void setSubtype(String subtype) {
		this.subtype = subtype;
	}

	public String getString1() {
		return string1;
	}

	public void setString1(String string1) {
		this.string1 = string1;
	}

	public String getString2() {
		return string2;
	}

	public void setString2(String string2) {
		this.string2 = string2;
	}

	public Long getInteger1() {
		return integer1;
	}

	public void setInteger1(Long integer1) {
		this.integer1 = integer1;
	}

	public Long getInteger2() {
		return integer2;
	}

	public void setInteger2(Long integer2) {
		this.integer2 = integer2;
	}

	public Double getDouble1() {
		return double1;
	}

	public void setDouble1(Double double1) {
		this.double1 = double1;
	}

	public Double getDouble2() {
		return double2;
	}

	public void setDouble2(Double double2) {
		this.double2 = double2;
	}

	public Date getDate1() {
		return date1;
	}

	public void setDate1(Date date1) {
		this.date1 = date1;
	}

	public Date getDate2() {
		return date2;
	}

	public void setDate2(Date date2) {
		this.date2 = date2;
	}

	@Override
	public int compareTo(Generic o) {
		if (getType().compareTo(o.getType()) != 0)
			return getType().compareTo(o.getType());
		else
			return getSubtype().compareTo(o.getSubtype());
	}

	public String getDisplayString1() {
		return StringUtils.abbreviate(getString1(), 65);
	}

	public String getString3() {
		return string3;
	}

	public void setString3(String string3) {
		this.string3 = string3;
	}

	public Long getInteger3() {
		return integer3;
	}

	public void setInteger3(Long integer3) {
		this.integer3 = integer3;
	}

	public Long getQualifier() {
		return qualifier;
	}

	public void setQualifier(Long qualifier) {
		this.qualifier = qualifier;
	}

	public String getString4() {
		return string4;
	}

	public void setString4(String string4) {
		this.string4 = string4;
	}
}