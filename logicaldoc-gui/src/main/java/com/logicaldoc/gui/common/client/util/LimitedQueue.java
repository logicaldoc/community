package com.logicaldoc.gui.common.client.util;

import java.util.LinkedList;

/**
 * A limited queue
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 *
 * @param <E>  the entity to use
 */
public class LimitedQueue<E> extends LinkedList<E> {
	private static final long serialVersionUID = 1L;
	private int limit;

	public LimitedQueue(int limit) {
		this.limit = limit;
	}

	@Override
	public boolean add(E o) {
		super.add(o);
		while (size() > limit) {
			super.remove();
		}
		return true;
	}
}
