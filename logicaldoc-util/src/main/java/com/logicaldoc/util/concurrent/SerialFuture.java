package com.logicaldoc.util.concurrent;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * A future returns null but waits for the completion of a given collection of
 * futures
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 * @param <V> Type of all the futures to wait for
 */
public class SerialFuture<V> implements Future<V> {

	private List<Future<V>> futures = new ArrayList<>();

	public SerialFuture(Collection<Future<V>> futures) {
		for (Future<V> future : futures)
			this.futures.add(future);
	}

	@Override
	public boolean cancel(boolean mayInterruptIfRunning) {
		return futures.stream().allMatch(f -> f.cancel(mayInterruptIfRunning));
	}

	@Override
	public boolean isCancelled() {
		return futures.stream().allMatch(Future::isCancelled);
	}

	@Override
	public boolean isDone() {
		return futures.stream().allMatch(Future::isDone);
	}

	@Override
	public V get() throws InterruptedException, ExecutionException {
		return getAll().stream().findFirst().orElse(null);
	}

	public List<V> getAll() throws InterruptedException, ExecutionException {
		List<V> gets = new ArrayList<>();
		for (Future<V> future : futures)
			gets.add(future.get());
		return gets;
	}

	@Override
	public V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		return getAll(timeout, unit).stream().findFirst().orElse(null);
	}

	public List<V> getAll(long timeout, TimeUnit unit)
			throws InterruptedException, ExecutionException, TimeoutException {
		List<V> gets = new ArrayList<>();
		for (Future<V> future : futures)
			gets.add(future.get(timeout, unit));
		return gets;
	}

}