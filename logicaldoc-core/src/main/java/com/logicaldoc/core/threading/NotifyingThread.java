package com.logicaldoc.core.threading;

import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CopyOnWriteArraySet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.time.TimeDiff;

/**
 * A Thread that notifies it's listeners about the end of the elaboration and
 * also measures the execution time.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 *
 * @param <T> the type of callable
 */
public class NotifyingThread<T> implements Callable<T> {

	private static final Logger log = LoggerFactory.getLogger(NotifyingThread.class);

	/**
	 * The start time in milliseconds
	 */
	private long startTime = 0;

	/**
	 * When the process ended in milliseconds
	 */
	private long endTime = 0;

	/**
	 * List of listeners
	 */
	private final Set<ThreadCompleteListener<T>> listeners = new CopyOnWriteArraySet<>();

	/**
	 * The error that stopped the execution
	 */
	private Throwable error;

	private Callable<T> wrappedCallable;

	private String name;

	/**
	 * Interface to be implemented by the listeners
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.5.3
	 */
	public interface ThreadCompleteListener<T> {
		public void completed(final NotifyingThread<T> thread);
	}

	public final void addListener(final ThreadCompleteListener<T> listener) {
		listeners.add(listener);
	}

	public final void removeListener(final ThreadCompleteListener<T> listener) {
		listeners.remove(listener);
	}

	private final void notifyListeners() {
		for (ThreadCompleteListener<T> listener : listeners) {
			listener.completed(this);
		}
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Gets the elapsed time since the begin of the execution until now o until
	 * the end of the thread's execution
	 * 
	 * @return the elapsed time in milliseconds
	 */
	public long getElapsedTime() {
		long elapsed = 0;
		if (endTime > 0)
			elapsed = endTime - startTime;
		else if (startTime > 0)
			elapsed = System.currentTimeMillis() - startTime;
		if (elapsed < 0)
			elapsed = 0;

		return elapsed;
	}

	/**
	 * Checks if the thread has finished
	 * 
	 * @return if the it has finished
	 */
	public boolean isOver() {
		return endTime > 0;
	}

	@Override
	public final T call() {
		try {
			endTime = 0;
			startTime = System.currentTimeMillis();
			error = null;
			log.debug("Thread {} started", getName());
			if (wrappedCallable != null) {
				return wrappedCallable.call();
			} else {
				return doRun();
			}
		} catch (Exception t) {
			error = t;
			log.debug("Thread {} ended in error after {}", getName(), TimeDiff.printDuration(getElapsedTime()), t);
		} finally {
			endTime = System.currentTimeMillis();
			notifyListeners();
		}
		return null;
	}

	public NotifyingThread(String name) {
		super();
		this.name = name;
	}

	public NotifyingThread(Callable<T> wrappedCallable, String name) {
		this.wrappedCallable = wrappedCallable;
		this.name = name;
	}

	/**
	 * Concrete implementations should override this method to implement their
	 * own processing.
	 */
	public T doRun() {
		return null;
	}

	public Throwable getError() {
		return error;
	}
}
