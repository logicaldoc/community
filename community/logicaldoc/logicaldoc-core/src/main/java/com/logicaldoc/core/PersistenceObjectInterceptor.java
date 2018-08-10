package com.logicaldoc.core;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;

import org.hibernate.EmptyInterceptor;
import org.hibernate.type.Type;

/**
 * Takes care about updating the last modified date of a persistence object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9 
 */
public class PersistenceObjectInterceptor extends EmptyInterceptor {

	protected static final String LAST_MODIFIED = "lastModified";

	private static final long serialVersionUID = 1L;

	public PersistenceObjectInterceptor() {
	}

	@Override
	public boolean onFlushDirty(Object entity, Serializable id, Object[] currentState, Object[] previousState,
			String[] propertyNames, Type[] types) {
		return onSave(entity, id, currentState, propertyNames, types);
	}

	@Override
	public boolean onSave(Object entity, Serializable id, Object[] currentState, String[] propertyNames, Type[] types) {
		if (entity instanceof PersistentObject) {
			boolean changed = setValue(currentState, propertyNames, LAST_MODIFIED, new Date());
			return changed;
		}
		return false;
	}

	protected boolean setValue(Object[] currentState, String[] propertyNames, String propertyToSet, Object value) {
		int index = Arrays.asList(propertyNames).indexOf(propertyToSet);
		if (index >= 0) {
			currentState[index] = value;
			return true;
		} else
			return false;
	}
}
