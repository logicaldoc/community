package com.logicaldoc.util.concurrent;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * A future that wraps an other future representing some kind of elaboration on a specified object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 * @param <V> Type of the wrapped future
 * @param <T> Type of the object being elaborated
 */
public class FutureElaboration<T, V> implements Future<V> {

	private T object;

	private Future<V> future;

	public FutureElaboration(T object, Future<V> future) {
		this.object = object;
		this.future = future;
	}

	@Override
	public boolean cancel(boolean mayInterruptIfRunning) {
		return future.cancel(mayInterruptIfRunning);
	}

	@Override
	public boolean isCancelled() {
		return future.isCancelled();
	}

	@Override
	public boolean isDone() {
		return future.isDone();
	}

	@Override
	public V get() throws InterruptedException, ExecutionException {
		return future.get();
	}

	@Override
	public V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		return future.get(timeout, unit);
	}

	public T getObject() {
		return object;
	}
}