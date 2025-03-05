package com.logicaldoc.core;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;
import org.hibernate.annotations.Where;

/**
 * This abstract class defines the minimum requirements of persistent objects.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@MappedSuperclass
@Where(clause = "ld_deleted=0")
public abstract class PersistentObject implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * This is used to mark a deletion that must be shown in the trash bin
	 */
	public static final int DELETED_CODE_DEFAULT = 1;

	/**
	 * This is used to mark a deletion that must be physically removed
	 */
	public static final int DELETED_CODE_STRONG = 2;

	@Id
	@GeneratedValue(generator = "hilo")
	@GenericGenerator(name = "hilo", strategy = "enhanced-table", parameters = {
			@Parameter(name = "table_name", value = "hibernate_sequences"),
			@Parameter(name = "prefer_entity_table_as_segment_value", value = "true"),
			@Parameter(name = "optimizer", value = "org.hibernate.id.enhanced.HiLoOptimizer"),
			@Parameter(name = "initial_value", value = "100") })
	@Column(name = "ld_id")
	public long id = 0;

	@Column(name = "ld_tenantid", nullable = false)
	private long tenantId = 1L;

	@Column(name = "ld_deleted", nullable = false)
	private int deleted = 0;

	@Column(name = "ld_lastmodified", nullable = false)
	private Date lastModified = new Date();

	@Column(name = "ld_creation", nullable = false)
	private Date creation = new Date();

	@Version
	@Column(name = "ld_recordversion", nullable = false)
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

	/**
	 * The object's creation date
	 * 
	 * @return the creation date
	 */
	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (id ^ (id >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		PersistentObject other = (PersistentObject) obj;
		return id == other.id;
	}
}