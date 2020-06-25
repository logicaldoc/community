package com.logicaldoc.core;

import java.util.Date;

/**
 * This abstract class defines the minimum requirements of persistent objects.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public abstract class PersistentObject {
	/**
	 * This is used to mark a deletion that must be shown in the trash bin
	 */
	public final static int DELETED_CODE_DEFAULT = 1;

	/**
	 * This is used to mark a deletion that must be physically removed
	 */
	public final static int DELETED_CODE_STRONG = 2;

	private long id = 0;

	private long tenantId = 1L;

	private int deleted = 0;

	private Date lastModified = new Date();

	private long recordVersion = 0L;

	/**
	 * Unique identifier in the data store
	 * 
	 * @return the unique identifier of this record
	 */
	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	/**
	 * The last time this instance was modified
	 * 
	 * @return the last modified date
	 */
	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	/**
	 * This flag is used to mark an object as deleted
	 * 
	 * @return <b>1</b> if the record is deleted, <b>0</b> otherwise
	 */
	public int getDeleted() {
		return deleted;
	}

	public void setDeleted(int deleted) {
		this.deleted = deleted;
	}

	public boolean equals(Object obj) {
		if (!(obj instanceof PersistentObject))
			return false;
		PersistentObject other = (PersistentObject) obj;
		return other.getId() == this.getId();
	}

	public int hashCode() {
		return Long.valueOf(getId()).hashCode();
	}

	public String toString() {
		return Long.toString(getId());
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	public long getRecordVersion() {
		return recordVersion;
	}

	public void setRecordVersion(long recordVersion) {
		this.recordVersion = recordVersion;
	}
}