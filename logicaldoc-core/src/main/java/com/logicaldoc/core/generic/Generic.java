package com.logicaldoc.core.generic;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MapKeyColumn;
import jakarta.persistence.OrderBy;
import jakarta.persistence.Table;

/**
 * Instances of this class represents generic informations in the database. Use
 * this Business entity to store configurations or stuffs like this. <br>
 * 
 * Each Generic is identified by a type and subtype
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Entity
@Table(name = "ld_generic")
@Cacheable
public class Generic extends ExtensibleObject implements Comparable<Generic> {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_type", length = 255, nullable = false)
	private String type;

	@Column(name = "ld_subtype", length = 255, nullable = false)
	private String subtype;

	@Column(name = "ld_qualifier")
	private Long qualifier;

	@Column(name = "ld_string1")
	private String string1;

	@Column(name = "ld_string2")
	private String string2;

	@Column(name = "ld_string3")
	private String string3;

	@Column(name = "ld_string4")
	private String string4;

	@Column(name = "ld_string5")
	private String string5;

	@Column(name = "ld_string6")
	private String string6;

	@Column(name = "ld_string7")
	private String string7;

	@Column(name = "ld_string8")
	private String string8;

	@Column(name = "ld_text1")
	private String text1;

	@Column(name = "ld_integer1")
	private Long integer1;

	@Column(name = "ld_integer2")
	private Long integer2;

	@Column(name = "ld_integer3")
	private Long integer3;

	@Column(name = "ld_double1")
	private Double double1;

	@Column(name = "ld_double2")
	private Double double2;

	@Column(name = "ld_date1", columnDefinition = "DATETIME(3)")
	private Date date1;

	@Column(name = "ld_date2", columnDefinition = "DATETIME(3)")
	private Date date2;

	@ElementCollection
	@CollectionTable(name = "ld_generic_ext", joinColumns = @JoinColumn(name = "ld_genid"))
	@MapKeyColumn(name = "ld_name", length = 255)
	@OrderBy("ld_position ASC, ld_name ASC")
	private Map<String, Attribute> attributes = new HashMap<>();

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

	public String getString5() {
		return string5;
	}

	public void setString5(String string5) {
		this.string5 = string5;
	}

	public String getString6() {
		return string6;
	}

	public void setString6(String string6) {
		this.string6 = string6;
	}

	public String getString7() {
		return string7;
	}

	public void setString7(String string7) {
		this.string7 = string7;
	}

	public String getString8() {
		return string8;
	}

	public void setString8(String string8) {
		this.string8 = string8;
	}

	public String getText1() {
		return text1;
	}

	public void setText1(String text1) {
		this.text1 = text1;
	}

	@Override
	public Map<String, Attribute> getAttributes() {
		return attributes;
	}

	@Override
	public void setAttributes(Map<String, Attribute> attributes) {
		this.attributes = attributes;
	}

	@Override
	public int compareTo(Generic o) {
		if (equals(o))
			return 0;

		if (getType().compareTo(o.getType()) != 0)
			return getType().compareTo(o.getType());
		else
			return getSubtype().compareTo(o.getSubtype());
	}

	@Override
	public Long getTemplateId() {
		return null;
	}

	@Override
	public void setTemplateId(Long templateId) {
		// Not implemented
	}

	@Override
	public String getTemplateName() {
		return null;
	}

	@Override
	public void setTemplateName(String templateName) {
		// Not implemented
	}

	@Override
	public Template getTemplate() {
		return null;
	}

	@Override
	public void setTemplate(Template template) {
		// not implemented
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((qualifier == null) ? 0 : qualifier.hashCode());
		result = prime * result + ((subtype == null) ? 0 : subtype.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
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
		Generic other = (Generic) obj;
		if (qualifier == null) {
			if (other.qualifier != null)
				return false;
		} else if (!qualifier.equals(other.qualifier))
			return false;
		if (subtype == null) {
			if (other.subtype != null)
				return false;
		} else if (!subtype.equals(other.subtype))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

}