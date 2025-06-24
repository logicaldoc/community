package com.logicaldoc.core;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;

import org.hibernate.Interceptor;
import org.hibernate.type.Type;

/**
 * An {@link Interceptor} that takes care about updating the last modified date of a persistence object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class LastModifiedInterceptor implements Interceptor
{

	protected static final String LAST_MODIFIED = "lastModified";


	public LastModifiedInterceptor() {
		super();
	}

	@Override
	public boolean onFlushDirty(Object entity, Serializable id, Object[] currentState, Object[] previousState,
			String[] propertyNames, Type[] types) {
		return onSave(entity, id, currentState, propertyNames, types);
	}

	@Override
	public boolean onSave(Object entity, Serializable id, Object[] currentState, String[] propertyNames, Type[] types) {
		if (entity instanceof PersistentObject)
			return setValue(currentState, propertyNames, LAST_MODIFIED, new Date());
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