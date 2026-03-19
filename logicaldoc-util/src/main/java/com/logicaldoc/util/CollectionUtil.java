package com.logicaldoc.util;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Some utility methods to handle collections.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
public class CollectionUtil {

    private CollectionUtil() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Divide a list to segments of n size
     * 
     * @param <T> the type of elements in <code>list</code>
     * @param list the collection to process
     * @param size number of elements in each segment
     * 
     * @return the collection of segments
     */
    public static <T> Collection<List<T>> partition(List<T> list, int size) {
        final AtomicInteger counter = new AtomicInteger(0);
        return list.stream().collect(Collectors.groupingBy(it -> counter.getAndIncrement() / (size > 0 ? size : 1)))
                .values();
    }

    /**
     * Reduces a collection of elements into a comma-separated string of
     * elements
     * 
     * @param <T> Type of elements in the collection
     * @param list Collection of elements
     * 
     * @return String of delimited elements
     */
    public static <T> String join(Collection<T> list) {
        return join(list, ",");
    }

    /**
     * Reduces a collection of elements into a string with elements separated by
     * a delimiter
     * 
     * @param <T> Type of elements in the collection
     * @param list Collection of elements
     * @param delimiter Separator to use
     * 
     * @return String of delimited elements
     */
    public static <T> String join(Collection<T> list, String delimiter) {
        return join(list, Object::toString, delimiter);
    }

    /**
     * Reduces a collection of elements into a comma-separated string
     * 
     * @param <T> Type of elements in the collection
     * @param list Collection of elements
     * @param mapper Function to map an element into its string representation
     * 
     * @return Comma separated String of elements
     */
    public static <T> String join(Collection<T> list, Function<? super T, String> mapper) {
        return join(list, mapper, ",");
    }

    /**
     * Reduces a collection of elements into a string with elements separated by
     * a delimiter
     * 
     * @param <T> Type of elements in the collection
     * @param list Collection of elements
     * @param mapper Function to map an element into its string representation
     * @param delimiter Separator to use
     * 
     * @return String of delimited elements
     */
    public static <T> String join(Collection<T> list, Function<? super T, String> mapper, String delimiter) {
        return list.stream().map(mapper).collect(Collectors.joining(delimiter));
    }
}