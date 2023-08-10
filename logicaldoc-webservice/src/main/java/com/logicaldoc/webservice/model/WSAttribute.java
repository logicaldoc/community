package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.Date;
import java.util.GregorianCalendar;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;

import com.logicaldoc.util.time.DateUtil;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Extended attribute of a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
@XmlRootElement(name = "attribute")
@XmlType(name = "WSAttribute")
public class WSAttribute implements Serializable {

	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(documented = false)
	public static final int TYPE_STRING = 0;

	@WSDoc(documented = false)
	public static final int TYPE_INT = 1;

	@WSDoc(documented = false)
	public static final int TYPE_DOUBLE = 2;

	@WSDoc(documented = false)
	public static final int TYPE_DATE = 3;

	@WSDoc(documented = false)
	public static final int TYPE_USER = 4;

	@WSDoc(documented = false)
	public static final int TYPE_BOOLEAN = 5;

	@WSDoc(documented = false)
	public static final int TYPE_FOLDER = 6;

	@WSDoc(required = true, description = "name of the attribute")
	private String name;

	@WSDoc(required = false)
	private String stringValue;

	@WSDoc(required = false)
	private Long intValue;

	@WSDoc(required = false)
	private Double doubleValue;

	@WSDoc(required = false, description = "the date value; format must be 'yyyy-MM-dd'")
	private String dateValue;

	@WSDoc(required = true, description = "<b>0</b> = String, <b>1</b> = int, <b>2</b> = double, <b>3</b> = date, <b>4</b> = user (intValue represents the user's id), <b>5</b> = boolean (intValue must be <b>0</b> or <b>1</b>), <b>6</b> = folder (intValue represents the folders's id)")
	private int type = TYPE_STRING;

	@WSDoc(required = true)
	private int mandatory = 0;

	@WSDoc(required = true)
	private int hidden = 0;

	@WSDoc(required = true)
	private int readonly = 0;

	@WSDoc(required = false)
	private int position = 0;

	@WSDoc(required = false)
	private String label;

	@WSDoc(required = true, description = "<b>0</b> = free, <b>1</b> = preset")
	private int editor = 0;

	@WSDoc(required = false, description = "Id of the attribute set")
	private Long setId;

	@WSDoc(required = true, description = "<b>0</b> = single value, <b>1</b> = one or more values")
	private int multiple = 0;

	@WSDoc(required = false, description = "The reference attribute name, used for multi value attributes")
	private String parent;

	@WSDoc(required = false, description = "Name of another attribute on which the value of this attribute also depends, used for managing linked presets")
	private String dependsOn;

	@WSDoc(required = false, description = "Read only. In case of multiple string values, contains the values separated by a comma")
	private String stringValues;

	@WSDoc(required = false, description = "Optional validation script")
	private String validation;

	@WSDoc(required = false, description = "Optional initialization script")
	private String initialization;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		Object val = WSAttribute.getValue(this);
		return getName() + (val != null ? (" - " + val.toString()) : "");
	}

	public String getStringValue() {
		return stringValue;
	}

	public void setStringValue(String stringValue) {
		this.stringValue = stringValue;
	}

	public Long getIntValue() {
		return intValue;
	}

	public void setIntValue(Long intValue) {
		this.intValue = intValue;
	}

	public Double getDoubleValue() {
		return doubleValue;
	}

	public void setDoubleValue(Double doubleValue) {
		this.doubleValue = doubleValue;
	}

	public String getDateValue() {
		return dateValue;
	}

	public void setDateValue(String dateValue) {
		this.dateValue = dateValue;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public int getMandatory() {
		return mandatory;
	}

	public void setMandatory(int mandatory) {
		this.mandatory = mandatory;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * Gets the attribute value. It can be as String, Long, Double or Date. We
	 * declare this static because do not want the value field to be serialized
	 * in SOAP / REST
	 * 
	 * @param attribute the attribute instance
	 * 
	 * @return The attribute value as Object.
	 */
	public static Object getValue(WSAttribute attribute) {
		switch (attribute.type) {
		case TYPE_STRING:
			return attribute.getStringValue();
		case TYPE_INT:
			return attribute.getIntValue();
		case TYPE_BOOLEAN:
			return attribute.getIntValue();
		case TYPE_DOUBLE:
			return attribute.getDoubleValue();
		case TYPE_DATE:
			return WSUtil.convertStringToDate(attribute.getDateValue());
		case TYPE_USER:
			return attribute.getIntValue();
		case TYPE_FOLDER:
			return attribute.getIntValue();
		default:
			return attribute.getStringValue();
		}
	}

	/**
	 * Sets the attribute value. It can be as String, Long, Double or Date.
	 * 
	 * @param attribute the attribute instance
	 * @param value The attribute value
	 */
	public static void setValue(WSAttribute attribute, Object value) {
		if (attribute.getType() == WSAttribute.TYPE_USER && !(value instanceof WSUser)) {
			/*
			 * Needed to fix JAXB logic that will invoke getValue(that returns a
			 * Long) and setValue
			 */
			if (value instanceof Long)
				attribute.intValue = (Long) value;
			else if (value instanceof String)
				attribute.stringValue = (String) value;
			return;
		}

		setNonUserValue(attribute, value);
	}

	private static void setNonUserValue(WSAttribute attribute, Object value) {
		if (value instanceof String) {
			attribute.type = TYPE_STRING;
			attribute.setStringValue((String) value);
		} else if (value instanceof Long) {
			attribute.type = TYPE_INT;
			attribute.setIntValue((Long) value);
		} else if (value instanceof Integer) {
			attribute.type = TYPE_INT;
			attribute.setIntValue(((Integer) value).longValue());
		} else if (value instanceof Boolean) {
			attribute.setIntValue(((Boolean) value).booleanValue() ? 1L : 0L);
			attribute.type = TYPE_BOOLEAN;
		} else if (value instanceof Double) {
			attribute.type = TYPE_DOUBLE;
			attribute.setDoubleValue((Double) value);
		} else if (value instanceof Date) {
			attribute.type = TYPE_DATE;
			attribute.setDateValue(DateUtil.format((Date) value));
		} else if (value instanceof WSUser) {
			attribute.stringValue = ((WSUser) value).getFullName();
			attribute.intValue = ((WSUser) value).getId();
			attribute.type = TYPE_USER;
		} else if (value instanceof WSFolder) {
			attribute.stringValue = ((WSFolder) value).getName();
			attribute.intValue = ((WSFolder) value).getId();
			attribute.type = TYPE_FOLDER;
		} else {
			setDateValue(attribute, value);
		}
	}

	private static void setDateValue(WSAttribute attribute, Object value) {
		attribute.type = TYPE_DATE;
		if (value != null && value instanceof XMLGregorianCalendar) {
			XMLGregorianCalendar theXGCal = (XMLGregorianCalendar) value;
			GregorianCalendar theGCal = theXGCal.toGregorianCalendar();
			Date theDate = theGCal.getTime();
			attribute.setDateValue(DateUtil.format(theDate));
		} else if (value != null && value instanceof Date) {
			attribute.setDateValue(DateUtil.format((Date) value));
		} else
			attribute.setDateValue(null);
	}

	public int getEditor() {
		return editor;
	}

	public void setEditor(int editor) {
		this.editor = editor;
	}

	public Long getSetId() {
		return setId;
	}

	public void setSetId(Long setId) {
		this.setId = setId;
	}

	public int getHidden() {
		return hidden;
	}

	public void setHidden(int hidden) {
		this.hidden = hidden;
	}

	public int getMultiple() {
		return multiple;
	}

	public String getParent() {
		return parent;
	}

	public void setMultiple(int multiple) {
		this.multiple = multiple;
	}

	public void setParent(String parent) {
		this.parent = parent;
	}

	public String getStringValues() {
		return stringValues;
	}

	public void setStringValues(String stringValues) {
		this.stringValues = stringValues;
	}

	public String getDependsOn() {
		return dependsOn;
	}

	public void setDependsOn(String dependsOn) {
		this.dependsOn = dependsOn;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}

	public String getInitialization() {
		return initialization;
	}

	public void setInitialization(String initialization) {
		this.initialization = initialization;
	}
	
	public int getReadonly() {
		return readonly;
	}

	public void setReadonly(int readonly) {
		this.readonly = readonly;
	}
}