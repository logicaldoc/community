package com.logicaldoc.core.metadata;

import java.util.Date;

import com.logicaldoc.core.security.User;

/**
 * This class defines the value of an attribute associated to an extensible
 * object. For each value, is possible to define the type and if it is mandatory
 * or not.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 4.5.1
 */
public class Attribute implements Comparable<Attribute> {

	public static final int TYPE_STRING = 0;

	public static final int TYPE_INT = 1;

	public static final int TYPE_DOUBLE = 2;

	public static final int TYPE_DATE = 3;

	public static final int TYPE_USER = 4;

	public static final int TYPE_BOOLEAN = 5;

	public static final int EDITOR_DEFAULT = 0;

	public static final int EDITOR_LISTBOX = 1;

	private String label;

	private String stringValue;

	private Long intValue;

	private Double doubleValue;

	private Date dateValue;

	private int type = TYPE_STRING;

	private int mandatory = 0;

	private int position = 0;

	private int editor = EDITOR_DEFAULT;

	/**
	 * Reference to the Attribute Set
	 */
	private Long setId;

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
		if (value instanceof String) {
			this.type = TYPE_STRING;
			setStringValue((String) value);
		} else if (value instanceof Integer) {
			this.type = TYPE_INT;
			setIntValue(new Long((Integer) value));
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
			this.stringValue = ((User) value).getFullName();
		} else if (value instanceof Boolean) {
			this.type = TYPE_BOOLEAN;
			this.intValue = ((Boolean) value).booleanValue() ? 1L : 0L;
		} else {
			throw new IllegalArgumentException("No a String, Long, Double, Date, Boolean or User value");
		}
	}

	/**
	 * Whether an attribute value is mandatory or not.
	 * 
	 * @return If 0, the attribute value is not mandatory; if 1, the attribute
	 *         value is mandatory.
	 */
	public int getMandatory() {
		return mandatory;
	}

	public void setMandatory(int mandatory) {
		this.mandatory = mandatory;
	}

	/**
	 * This is the position of the attribute into the attributes list.
	 */
	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	@Override
	public int compareTo(Attribute o) {
		return new Integer(getPosition()).compareTo(o.getPosition());
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

	@Override
	public Object clone() throws CloneNotSupportedException {
		Attribute clone = new Attribute();
		clone.setStringValue(stringValue);
		clone.setDateValue(dateValue);
		clone.setDoubleValue(doubleValue);
		clone.setIntValue(intValue);
		clone.setLabel(label);
		clone.setMandatory(mandatory);
		clone.setPosition(position);
		clone.setEditor(editor);
		clone.setType(type);
		clone.setSetId(setId);

		return clone;
	}

	public Long getSetId() {
		return setId;
	}

	public void setSetId(Long setId) {
		this.setId = setId;
	}
}