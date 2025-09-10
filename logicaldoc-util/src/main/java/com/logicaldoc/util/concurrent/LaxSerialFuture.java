package com.logicaldoc.util.concurrent;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * A future that waits for availability of a list of other futures and then
 * returns the value of the first future
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 * @param <V> Super type of all the futures to wait for
 */
public class LaxSerialFuture<V> implements Future<V> {

	private List<Future<? extends V>> futures = new ArrayList<>();

	public LaxSerialFuture(Collection<Future<? extends V>> futures) {
		for (Future<? extends V> future : futures)
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
		for (Future<? extends V> future : futures)
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
		for (Future<? extends V> future : futures)
			gets.add(future.get(timeout, unit));
		return gets;
	}

}