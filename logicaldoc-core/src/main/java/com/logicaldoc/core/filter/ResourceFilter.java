package com.logicaldoc.core.filter;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.Value;

/**
 * Implementations of this interface can elaborate the original document's
 * resource to produce something different into a target file.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.2
 */
public interface ResourceFilter {

    /**
     * Invoked during the download with the purpose to produce an altered
     * version of the content
     * 
     * @param resource The exact resource to filter
     * @param context The current context
     * @param transaction The current session information
     * @param targetFilename Holds the final filename to give to target file and
     *        can be changed
     * @param target The file to write the elaborated content to
     * @param filterDictionary A filter dictionary to pass through all the
     *        filters
     * 
     * @throws IOException I/O error
     * @throws PersistenceException Error in the data layer
     */
    public void filter(
            StoreResource resource,
            String context,
            History transaction,
            Value<String> targetFilename,
            File target,
            Map<String, Object> filterDictionary) throws IOException, PersistenceException;
}