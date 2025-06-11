package com.logicaldoc.core.sequence;

import java.util.Date;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;

/**
 * A sequence in the database
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
@Entity
@Table(name = "ld_sequence")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Sequence extends PersistentObject {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_objectid", nullable = false)
	private long objectId = 0L;
	
	@Column(name = "ld_name", nullable = false)
	private String name;

	@Column(name = "ld_lastreset")
	private Date lastReset = new Date();

	@Column(name = "ld_value", nullable = false)
	private long value = 0L;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Date getLastReset() {
		return lastReset;
	}

	public void setLastReset(Date lastReset) {
		this.lastReset = lastReset;
	}

	public long getValue() {
		return value;
	}

	public void setValue(long value) {
		this.value = value;
	}

	public long getObjectId() {
		return objectId;
	}

	public void setObjectId(long objectId) {
		this.objectId = objectId;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + (int) (objectId ^ (objectId >>> 32));
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
		Sequence other = (Sequence) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return objectId == other.objectId;
	}
}