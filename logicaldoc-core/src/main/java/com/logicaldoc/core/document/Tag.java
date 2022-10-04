package com.logicaldoc.core.document;

import java.io.Serializable;

import com.logicaldoc.core.security.Tenant;

/**
 * Represents a tag of a specific document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class Tag implements Comparable<Tag>, Serializable {

	private static final long serialVersionUID = 1L;

	private String tag;

	private long tenantId = Tenant.DEFAULT_ID;

	public Tag() {

	}

	public Tag(long tenantId, String tag) {
		this.tenantId = tenantId;
		this.tag = tag;
	}

	@Override
	public int compareTo(Tag o) {
		int comp = this.tag.compareTo(o.tag);
		if (comp != 0)
			return comp;
		return Long.compare(this.tenantId, o.tenantId);
	}

	public String getTag() {
		return tag;
	}

	public void setTag(String tag) {
		this.tag = tag;
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	@Override
	public String toString() {
		if (tag == null)
			return "";
		else
			return tag.toString();
	}

	@Override
	public int hashCode() {
		if (tag == null)
			return 0;
		else
			return (tag + tenantId).hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o == null)
			return false;

		if (this.getClass() != o.getClass())
			return false;

		Tag other = (Tag) o;
		return other.tenantId == this.tenantId && this.tag != null && this.tag.equals(other.tag);
	}
}
