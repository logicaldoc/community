package com.logicaldoc.core.metadata;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents an option for a multi-choice attribute
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.1
 */
public class AttributeOption extends PersistentObject implements Comparable<AttributeOption> {

	private long setId;

	private String attribute;

	private String value;

	private String label;

	private int position = 0;

	public AttributeOption() {
	}

	public AttributeOption(long setId, String attribute, String value) {
		this();
		this.setId = setId;
		this.attribute = attribute;
		this.value = value;
	}

	public long getSetId() {
		return setId;
	}

	public void setSetId(long setId) {
		this.setId = setId;
	}

	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	@Override
	public int compareTo(AttributeOption other) {
		return new Integer(position).compareTo(new Integer(other.position));
	}
}
