package com.logicaldoc.util;

import java.util.ArrayList;
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

	public static void main(String[] args) {
		List<Long> in = new ArrayList<Long>();
		in.add(1L);
		// in.add(2L);
		// in.add(3L);
		// in.add(4L);
		// in.add(5L);
		// in.add(6L);
		// in.add(7L);
		// in.add(8L);
		// in.add(9L);
		// in.add(10L);
		// in.add(11L);
		// in.add(12L);
		// in.add(13L);
		// in.add(14L);

		int size = (int) Math.ceil(in.size() / 2);
		System.out.println(size);
		System.out.println(partition(in, size));

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
