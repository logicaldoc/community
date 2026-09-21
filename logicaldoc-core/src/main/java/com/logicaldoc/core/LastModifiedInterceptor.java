package com.logicaldoc.core;

import java.util.Arrays;
import java.util.Date;

import org.hibernate.Interceptor;
import org.hibernate.type.Type;

/**
 * An {@link Interceptor} that takes care about updating the last modified date
 * of a persistence object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class LastModifiedInterceptor implements Interceptor {

    private static final String LAST_MODIFIED = "lastModified";

    @Override
    public boolean onFlushDirty(
            Object entity,
            Object id,
            Object[] currentState,
            Object[] previousState,
            String[] propertyNames,
            Type[] propertyTypes) {

        boolean modified = Interceptor.super.onFlushDirty(entity, id, currentState, previousState, propertyNames,
                propertyTypes);

        boolean lastModifiedUpdated = updateLastModified(entity, currentState, propertyNames);

        return modified || lastModifiedUpdated;
    }

    @Override
    public void onInsert(
            Object entity,
            Object id,
            Object[] currentState,
            String[] propertyNames,
            Type[] propertyTypes) {

        Interceptor.super.onInsert(entity, id, currentState, propertyNames, propertyTypes);

        updateLastModified(entity, currentState, propertyNames);
    }

    @Override
    public boolean onPersist(
            Object entity,
            Object id,
            Object[] currentState,
            String[] propertyNames,
            Type[] propertyTypes) {

        return updateLastModified(entity, currentState, propertyNames);
    }

    @Override
    public void onUpdate(
            Object entity,
            Object id,
            Object[] currentState,
            String[] propertyNames,
            Type[] propertyTypes) {

        Interceptor.super.onUpdate(entity, id, currentState, propertyNames, propertyTypes);

        updateLastModified(entity, currentState, propertyNames);
    }

    protected boolean setValue(Object[] currentState, String[] propertyNames, String propertyToSet, Object value) {

        int index = Arrays.asList(propertyNames).indexOf(propertyToSet);

        if (index < 0)
            return false;

        currentState[index] = value;
        return true;
    }

    private boolean updateLastModified(Object entity, Object[] currentState, String[] propertyNames) {

        return entity instanceof PersistentObject && setValue(currentState, propertyNames, LAST_MODIFIED, new Date());
    }
}