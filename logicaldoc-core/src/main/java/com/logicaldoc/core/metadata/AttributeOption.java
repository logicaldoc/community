package com.logicaldoc.core.metadata;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents an option for a multi-choice attribute
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AttributeOption extends PersistentObject implements Comparable<AttributeOption> {

	private static final long serialVersionUID = 1L;

	private long setId;

	private String attribute;

	/**
	 * An category, just to organize the values in groups
	 */
	private String category;

	/**
	 * The value of this option
	 */
	private String value;

	private String label;

	private int position = 0;

	public AttributeOption() {
	}

	public AttributeOption(long setId, String attribute, String value, String category) {
		this(setId, attribute, value);
		this.category = category;
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

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	@Override
	public int compareTo(AttributeOption other) {
		return Integer.compare(position, other.position);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AttributeOption))
			return false;
		AttributeOption other = (AttributeOption) obj;
		return other.getId() == this.getId();
	}
}