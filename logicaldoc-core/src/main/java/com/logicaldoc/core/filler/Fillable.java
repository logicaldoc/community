package com.logicaldoc.core.filler;

import com.logicaldoc.core.metadata.ExtensibleObject;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;

/**
 * An abstract extension of {@link ExtensibleObject} that adds the necessary to
 * support autofill
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 9.2.3
 */
@MappedSuperclass
public abstract class Fillable extends ExtensibleObject {

	private static final long serialVersionUID = 1L;

	/**
	 * ID of the filler to use for autofill operations
	 */
	@Column(name = "ld_fillerid", nullable = true)
	protected Long fillerId;

	public Long getFillerId() {
		return fillerId;
	}

	public void setFillerId(Long fillerId) {
		this.fillerId = fillerId;
	}
}