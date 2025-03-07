package com.logicaldoc.core.metadata;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;

/**
 * Represents an option for a multi-choice attribute
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
@Entity
@Table(name = "ld_extoption")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class AttributeOption extends PersistentObject implements Comparable<AttributeOption> {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_setid", nullable = false)
	private long setId;

	@Column(name = "ld_attribute", nullable = false)
	private String attribute;

	/**
	 * The value of this option
	 */
	@Column(name = "ld_value", nullable = false)
	private String value;
	
	/**
	 * An category, just to organize the values in groups
	 */
	@Column(name = "ld_category", nullable = false)
	private String category;

	@Column(name = "ld_label", nullable = false)
	private String label;

	@Column(name = "ld_position", nullable = false)
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
		if (equals(other))
			return 0;
		else
			return Integer.compare(position, other.position);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (int) (setId ^ (setId >>> 32));
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		AttributeOption other = (AttributeOption) obj;
		if (setId != other.setId)
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
}