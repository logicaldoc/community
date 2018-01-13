package com.logicaldoc.core.sequence;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * A sequence in the database
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.9
 */
public class Sequence extends PersistentObject {

	private String name;

	private Long objectId;

	private Date lastReset = new Date();

	private long value = 0L;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Long getObjectId() {
		return objectId;
	}

	public void setObjectId(Long objectId) {
		this.objectId = objectId;
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
}