package com.logicaldoc.util;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * Some utility methods to handle collections.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
public class CollectionUtil {

	private CollectionUtil() {
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
}
