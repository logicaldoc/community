package com.logicaldoc.core.sequence;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * A sequence in the database
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class Sequence extends PersistentObject {

	private static final long serialVersionUID = 1L;

	private String name;

	private long objectId = 0L;

	private Date lastReset = new Date();

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