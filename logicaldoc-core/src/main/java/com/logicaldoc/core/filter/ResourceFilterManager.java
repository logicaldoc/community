package com.logicaldoc.core.filter;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.Value;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

/**
 * Manager class used to handle format filters.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.2
 */
@Component("resourceFilterManager")
public class ResourceFilterManager implements ResourceFilter {

    /**
     * Gets the object available in the application context
     * 
     * @return the instance of this object in the application context
     */
    public static ResourceFilterManager get() {
        return Context.get(ResourceFilterManager.class);
    }

    private static final Logger log = LoggerFactory.getLogger(ResourceFilterManager.class);

    /**
     * Map context - filters
     */
    private Map<String, List<ResourceFilter>> filters = new HashMap<>();

    /**
     * Executes the filtering among all the filters configured for the given
     * context
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
            Map<String, Object> filterDictionary) throws IOException, PersistenceException {

        Map<String, Object> dict = new HashMap<>();
        if (filterDictionary != null)
            dict.putAll(filterDictionary);
        for (ResourceFilter filter : getFilters(context)) {
            filter.filter(resource, context, transaction, targetFilename, target, dict);
        }
    }

    /**
     * Initializes the converters map
     */
    public synchronized void init() {
        if (!filters.isEmpty())
            return;

        // Acquire the 'FormatConverter' extensions of the core plugin
        PluginRegistry registry = PluginRegistry.getInstance();
        Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "ResourceFilter").stream()
                .sorted((e1, e2) -> {
                    if (e1.getParameter("context").valueAsString().equals(e2.getParameter("context").valueAsString()))
                        return e1.getParameter("context").valueAsString()
                                .compareTo(e2.getParameter("context").valueAsString());
                    else
                        return Integer.compare(Integer.parseInt(e1.getParameter("position").valueAsString()),
                                Integer.parseInt(e2.getParameter("position").valueAsString()));
                }).toList();

        for (Extension ext : exts) {
            String className = ext.getParameter("class").valueAsString();

            // Try to instantiate the converter
            Object filter;
            try {
                Class<?> clazz = Class.forName(className);
                filter = clazz.getDeclaredConstructor().newInstance();
                if (!(filter instanceof ResourceFilter))
                    throw new ClassNotFoundException(String
                            .format("The specified filter %s doesn't implement ResourceFilter interface", className));
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException e) {
                log.warn(e.getMessage(), e);
                continue;
            }

            ResourceFilter documentFilter = (ResourceFilter) filter;
            String context = ext.getParameter("context").valueAsString().toLowerCase();
            updateFiltersMap(documentFilter, context);
            log.info("Registered resource filter {} for context {}", className, context);
        }
    }

    private void updateFiltersMap(ResourceFilter filter, String context) {
        List<ResourceFilter> fillersList = filters.computeIfAbsent(context, _ -> new ArrayList<>());
        if (!fillersList.contains(filter))
            fillersList.add(filter);
    }

    public List<ResourceFilter> getFilters(String context) {
        if (context == null)
            return List.of();
        return getFilters().getOrDefault(context.toLowerCase(), List.of());
    }

    private Map<String, List<ResourceFilter>> getFilters() {
        if (filters.isEmpty())
            init();
        return filters;
    }
}