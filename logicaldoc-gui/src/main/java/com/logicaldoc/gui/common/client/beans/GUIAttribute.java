package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * This class defines the value of an attribute associated to an extensible
 * object. For each value, is possible to define the type and if it is mandatory
 * or not.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.5.1
 */
public class GUIAttribute implements Comparable<GUIAttribute>, Serializable {

	private static final String[] FORBIDDEN_NAMES = new String[] { "date", "fileName", "fileSize", "creation", "creator",
			"version", "fileVersion", "published", "publisher", "name", "description" };

	private static final long serialVersionUID = 1L;

	public static final int TYPE_STRING_PRESET = -1;

	public static final int TYPE_STRING_TEXTAREA = -2;

	public static final int TYPE_STRING = 0;

	public static final int TYPE_INT = 1;

	public static final int TYPE_DOUBLE = 2;

	public static final int TYPE_DATE = 3;

	public static final int TYPE_USER = 4;

	public static final int TYPE_BOOLEAN = 5;

	public static final int TYPE_FOLDER = 6;

	public static final int EDITOR_DEFAULT = 0;

	public static final int EDITOR_LISTBOX = 1;

	public static final int EDITOR_TEXTAREA = 2;

	private int editor = EDITOR_DEFAULT;

	private String stringValue;

	/**
	 * In case of attribute of type User, this field stores the username
	 */
	private String username;

	private Long intValue;

	private Double doubleValue;

	private Date dateValue;

	private Boolean booleanValue;

	private int type = TYPE_STRING;

	private boolean mandatory = false;

	private boolean hidden = false;

	private boolean readonly = false;

	private boolean multiple = false;

	private String parent;

	private String dependsOn;

	private int position = 0;

	private String name;

	private String label;

	private String set;

	private Long setId;

	// Optional array of possible values
	private String[] options;

	private String stringValues;

	private String validation;

	private String initialization;

	public GUIAttribute() {
	}

	public GUIAttribute(GUIAttribute source) {
		this.editor = source.editor;
		this.stringValue = source.stringValue;
		this.username = source.username;
		this.intValue = source.intValue;
		this.doubleValue = source.doubleValue;
		this.dateValue = source.dateValue;
		this.booleanValue = source.booleanValue;
		this.type = source.type;
		this.mandatory = source.mandatory;
		this.hidden = source.hidden;
		this.readonly = source.readonly;
		this.multiple = source.multiple;
		this.parent = source.parent;
		this.dependsOn = source.dependsOn;
		this.position = source.position;
		this.name = source.name;
		this.label = source.label;
		this.set = source.set;
		this.setId = source.setId;
		this.options = source.options;
		this.stringValues = source.stringValues;
		this.validation = source.validation;
		this.initialization = source.initialization;
	}

	public static boolean isForbidden(String name) {
		for (String forbidden : FORBIDDEN_NAMES) {
			if (forbidden.equals(name))
				return true;
		}
		return false;
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

	public Date getDateValue() {
		return dateValue;
	}

	public void setDateValue(Date dateValue) {
		this.dateValue = dateValue;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public boolean isReadonly() {
		return readonly;
	}

	public void setReadonly(boolean readonly) {
		this.readonly = readonly;
	}

	/**
	 * Gets the attribute value. It can be as String, Long, Double or Date.
	 * 
	 * @return The attribute value as Object.
	 */
	public Object getValue() {
		switch (type) {
		case TYPE_INT:
			return getIntValue();
		case TYPE_DOUBLE:
			return getDoubleValue();
		case TYPE_DATE:
			return getDateValue();
		case TYPE_BOOLEAN:
			return getBooleanValue();
		case TYPE_USER:
			return getIntValue();
		case TYPE_FOLDER:
			return getIntValue();
		default:
			return getStringValue();
		}
	}

	/**
	 * Sets the attribute value. It can be as String, Long, Double or Date.
	 * 
	 * @param value The attribute value.
	 */
	public void setValue(Object value) {
		if (value instanceof java.lang.String) {
			this.type = TYPE_STRING;
			setStringValue((String) value);
		} else if (value instanceof Long) {
			this.type = TYPE_INT;
			setIntValue((Long) value);
		} else if (value instanceof Integer) {
			this.type = TYPE_INT;
			setIntValue(Long.parseLong(value.toString()));
		} else if (value instanceof Boolean) {
			this.type = TYPE_BOOLEAN;
			setBooleanValue((Boolean) value);
		} else if (value instanceof Double) {
			this.type = TYPE_DOUBLE;
			setDoubleValue((Double) value);
		} else if (value instanceof Date) {
			this.type = TYPE_DATE;
			setDateValue((Date) value);
		} else if (value instanceof GUIUser) {
			setIntValue(((GUIUser) value).getId());
			setStringValue(((GUIUser) value).getUsername());
			setUsername(((GUIUser) value).getUsername());
			this.type = TYPE_USER;
		} else if (value instanceof GUIFolder) {
			setIntValue(((GUIFolder) value).getId());
			setStringValue(((GUIFolder) value).getName());
			this.type = TYPE_FOLDER;
		} else if (value == null) {
			setStringValue(null);
			setDoubleValue(null);
			setIntValue(null);
			setDateValue(null);
			setBooleanValue(null);
		} else {
			throw new IllegalArgumentException(
					"Not a String, Long, Double, Boolean, Date, User or Folder value: " + value.getClass().getName());
		}
	}

	public boolean isMandatory() {
		return mandatory;
	}

	public void setMandatory(boolean mandatory) {
		this.mandatory = mandatory;
	}

	/**
	 * This is the position of the attribute into the attributes list
	 * 
	 * @return the position
	 */
	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	@Override
	public int compareTo(GUIAttribute o) {
		if (o.getPosition() == getPosition())
			return getName().compareTo(o.getName());
		else
			return Integer.compare(getPosition(), o.getPosition());
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof GUIAttribute))
			return false;
		GUIAttribute other = (GUIAttribute) obj;
		return getName().equals(other.getName()) && getPosition() == other.getPosition();
	}

	@Override
	public int hashCode() {
		return getName().hashCode();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
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

	public String[] getOptions() {
		return options;
	}

	public void setOptions(String[] options) {
		this.options = options;
	}

	public Boolean getBooleanValue() {
		return booleanValue;
	}

	public void setBooleanValue(Boolean booleanValue) {
		this.booleanValue = booleanValue;
	}

	public String getSet() {
		return set;
	}

	public void setSet(String set) {
		this.set = set;
	}

	public Long getSetId() {
		return setId;
	}

	public void setSetId(Long setId) {
		this.setId = setId;
	}

	public String getDisplayName() {
		String display = getName();
		if (getLabel() != null && !"".equals(getLabel()))
			display = getLabel();
		return display;
	}

	public boolean isHidden() {
		return hidden;
	}

	public void setHidden(boolean hidden) {
		this.hidden = hidden;
	}

	public boolean isMultiple() {
		return multiple;
	}

	public void setMultiple(boolean multiple) {
		this.multiple = multiple;
	}

	public String getParent() {
		return parent;
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

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
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
	public String toString() {
		return getName() + "=" + getValue();
	}

	public static String[] getForbiddenNames() {
		return FORBIDDEN_NAMES;
	}
}