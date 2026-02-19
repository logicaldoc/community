package com.logicaldoc.core.filler;

import java.util.Map;

import com.logicaldoc.core.history.History;

import jakarta.persistence.Cacheable;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;

/**
 * A Filler that does nothing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 *
 */
@Entity
@DiscriminatorValue("mock")
@Cacheable
public class MockFiller extends Filler {

	private static final long serialVersionUID = 1L;

	@Override
	public void fill(Fillable filler, String content, History transaction, Map<String, Object> dictionary) {
		// Do nothing
	}
}