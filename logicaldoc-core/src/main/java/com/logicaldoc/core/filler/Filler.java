package com.logicaldoc.core.filler;

import java.util.Map;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.metadata.ExtensibleObject;

import jakarta.persistence.Column;
import jakarta.persistence.DiscriminatorColumn;
import jakarta.persistence.DiscriminatorType;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

/**
 * A Filler implements its own logic to auto-fill the metadata of an
 * {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
@Entity
@Table(name = "ld_filler")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "ld_type", discriminatorType = DiscriminatorType.STRING)
public abstract class Filler extends PersistentObject {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_name", length = 255, nullable = false)
	private String name;

	@Column(name = "ld_label", length = 255, nullable = true)
	private String label;

	@Column(name = "ld_description", nullable = true)
	private String description;
	
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

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * Fills an object instance
	 * 
	 * @param object the instance to fill
	 * @param content the content of the object, if not specified it will be
	 *        taken from the transaction's file.
	 * @param transaction the current transaction
	 * @param dictionary Dictionary of the execution pipeline
	 */
	public abstract void fill(ExtensibleObject object, String content, History transaction,
			Map<String, Object> dictionary);

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((label == null) ? 0 : label.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		Filler other = (Filler) obj;
		if (label == null) {
			if (other.label != null)
				return false;
		} else if (!label.equals(other.label))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		}
		return name.equals(other.name);
	}

	@Override
	public String toString() {
		return "%s(%d)".formatted(name, id);
	}
}