package com.logicaldoc.core.metadata;

import java.util.Date;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.User;

/**
 * This class defines the value of an attribute associated to an extensible
 * object. For each value, is possible to define the type and if it is mandatory
 * or not.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.5.1
 */
public class Attribute implements Comparable<Attribute> {

	public static final int TYPE_STRING = 0;

	public static final int TYPE_INT = 1;

	public static final int TYPE_DOUBLE = 2;

	public static final int TYPE_DATE = 3;

	public static final int TYPE_USER = 4;

	public static final int TYPE_BOOLEAN = 5;

	public static final int TYPE_FOLDER = 6;

	public static final int EDITOR_DEFAULT = 0;

	public static final int EDITOR_LISTBOX = 1;

	/**
	 * Not persistent
	 */
	private String name;

	private String label;

	private String stringValue;

	/**
	 * String representation of the multiple string values
	 */
	private String stringValues;

	private Long intValue;

	private Double doubleValue;

	private Date dateValue;

	private int type = TYPE_STRING;

	private int hidden = 0;

	private int mandatory = 0;

	private int position = 0;

	private int editor = EDITOR_DEFAULT;

	/**
	 * Reference to the Attribute Set
	 */
	private Long setId;

	private int multiple = 0;

	/**
	 * Name of a parent attribute, used for multiple values attributes
	 */
	private String parent;

	/**
	 * Name of another attribute on which the value of this attribute also
	 * depends, used for managing linked presets
	 */
	private String dependsOn;

	/**
	 * Optional validation script
	 */
	private String validation;

	/**
	 * Optional script that defines the initial value
	 */
	private String initialization;

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

	public Date getDateValue() {
		return dateValue;
	}

	public void setDateValue(Date dateValue) {
		this.dateValue = dateValue;
	}

	public Boolean getBooleanValue() {
		if (intValue != null)
			return intValue.intValue() == 1;
		else
			return null;
	}

	public void setBooleanValue(Boolean booleanValue) {
		this.type = TYPE_BOOLEAN;
		if (booleanValue != null)
			this.intValue = booleanValue.booleanValue() ? 1L : 0L;
		else
			this.intValue = null;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	/**
	 * Gets the attribute value. It can be as String, Long, Double or Date.
	 * 
	 * @return The attribute value as Object.
	 */
	public Object getValue() {
		switch (type) {
		case TYPE_STRING:
			return getStringValue();
		case TYPE_INT:
			return getIntValue();
		case TYPE_DOUBLE:
			return getDoubleValue();
		case TYPE_DATE:
			return getDateValue();
		case TYPE_USER:
			return getIntValue();
		case TYPE_FOLDER:
			return getIntValue();
		case TYPE_BOOLEAN:
			if (getIntValue() == null)
				return null;
			else
				return getIntValue().intValue() == 1;
		}
		return null;
	}

	/**
	 * Sets the attribute value. It can be as String, Long, Double or Date.
	 * 
	 * @param value The attribute value.
	 */
	public void setValue(Object value) {
		if (value == null) {
			setStringValue(null);
			setIntValue(null);
			setDoubleValue(null);
			setDateValue(null);
		}

		if (value instanceof String) {
			this.type = TYPE_STRING;
			setStringValue((String) value);
		} else if (value instanceof Integer) {
			this.type = TYPE_INT;
			setIntValue(Long.valueOf((Integer) value));
		} else if (value instanceof Long) {
			this.type = TYPE_INT;
			setIntValue((Long) value);
		} else if (value instanceof Double) {
			this.type = TYPE_DOUBLE;
			setDoubleValue((Double) value);
		} else if (value instanceof Date) {
			this.type = TYPE_DATE;
			setDateValue((Date) value);
		} else if (value instanceof User) {
			this.type = TYPE_USER;
			this.intValue = ((User) value).getId();
			this.stringValue = ((User) value).getUsername();
		} else if (value instanceof Folder) {
			this.type = TYPE_FOLDER;
			this.intValue = ((Folder) value).getId();
			this.stringValue = ((Folder) value).getName();
		} else if (value instanceof Boolean) {
			this.type = TYPE_BOOLEAN;
			this.intValue = ((Boolean) value).booleanValue() ? 1L : 0L;
		} else {
			if (value != null)
				throw new IllegalArgumentException("No a String, Long, Double, Date, Boolean, User, Folder value");
		}
	}

	/**
	 * Whether an attribute value is mandatory or not.
	 * 
	 * @return If <b>0</b>, the attribute value is not mandatory; if <b>1</b>,
	 *         the attribute value is mandatory.
	 */
	public int getMandatory() {
		return mandatory;
	}

	public void setMandatory(int mandatory) {
		this.mandatory = mandatory;
	}

	/**
	 * This is the position of the attribute into the attributes list
	 * 
	 * @return the attribute's position
	 */
	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	@Override
	public int compareTo(Attribute o) {
		return Integer.valueOf(getPosition()).compareTo(o.getPosition());
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public int getEditor() {
		return editor;
	}

	public void setEditor(int editor) {
		this.editor = editor;
	}

	public int getHidden() {
		return hidden;
	}

	public void setHidden(int hidden) {
		this.hidden = hidden;
	}

	public Long getSetId() {
		return setId;
	}

	public void setSetId(Long setId) {
		this.setId = setId;
	}

	public int getMultiple() {
		return multiple;
	}

	public void setMultiple(int multiple) {
		this.multiple = multiple;
	}

	public String getParent() {
		return parent;
	}

	public void setParent(String parent) {
		this.parent = parent;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getStringValues() {
		return stringValues;
	}

	public void setStringValues(String stringValues) {
		this.stringValues = stringValues;
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

	public String getDependsOn() {
		return dependsOn;
	}

	public void setDependsOn(String dependsOn) {
		this.dependsOn = dependsOn;
	}

	@Override
	public Attribute clone() {
		Attribute clone = new Attribute();
		clone.setStringValue(stringValue);
		clone.setStringValues(stringValues);
		clone.setDateValue(dateValue);
		clone.setDoubleValue(doubleValue);
		clone.setIntValue(intValue);
		clone.setLabel(label);
		clone.setMandatory(mandatory);
		clone.setHidden(hidden);
		clone.setMultiple(multiple);
		clone.setPosition(position);
		clone.setEditor(editor);
		clone.setType(type);
		clone.setSetId(setId);
		clone.setParent(parent);
		clone.setName(name);
		clone.setValidation(validation);
		clone.setInitialization(initialization);
		clone.setDependsOn(dependsOn);
		return clone;
	}
}