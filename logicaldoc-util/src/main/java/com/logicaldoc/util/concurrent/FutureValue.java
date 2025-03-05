package com.logicaldoc.util.concurrent;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * A simple future that is always complete and always returns the given value
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 * @param <V> Type of future
 */
public class FutureValue<V> implements Future<V> {

	private V value;

	public FutureValue() {
		super();
	}

	public FutureValue(V value) {
		super();
		this.value = value;
	}

	@Override
	public boolean cancel(boolean mayInterruptIfRunning) {
		return false;
	}

	@Override
	public boolean isCancelled() {
		return false;
	}

	@Override
	public boolean isDone() {
		return true;
	}

	@Override
	public V get() throws InterruptedException, ExecutionException {
		return value;
	}

	@Override
	public V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		return value;
	}
}