package com.logicaldoc.core.document;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import com.logicaldoc.core.security.Tenant;

/**
 * Represents a tag of a specific document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
@Embeddable
public class Tag implements Comparable<Tag>, Serializable {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_tag", length = 255, nullable = false)
	private String tagWord;

	@Column(name = "ld_tenantid", nullable = false)
	private long tenantId = Tenant.DEFAULT_ID;

	public Tag() {

	}

	public Tag(long tenantId, String tag) {
		this.tenantId = tenantId;
		this.tagWord = tag;
	}

	@Override
	public int compareTo(Tag o) {
		int comp = this.tagWord.compareTo(o.tagWord);
		if (comp != 0)
			return comp;
		return Long.compare(this.tenantId, o.tenantId);
	}

	public String getTag() {
		return tagWord;
	}

	public void setTag(String tag) {
		this.tagWord = tag;
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	@Override
	public String toString() {
		if (tagWord == null)
			return "";
		else
			return tagWord;
	}

	@Override
	public int hashCode() {
		if (tagWord == null)
			return 0;
		else
			return (tagWord + tenantId).hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o == null)
			return false;

		if (this.getClass() != o.getClass())
			return false;

		Tag other = (Tag) o;
		return other.tenantId == this.tenantId && this.tagWord != null && this.tagWord.equals(other.tagWord);
	}
}
