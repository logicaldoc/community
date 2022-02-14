package com.logicaldoc.core.threading;

import java.util.Set;
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
 */
public class NotifyingThread extends Thread {

	private static Logger log = LoggerFactory.getLogger(NotifyingThread.class);

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
	private final Set<ThreadCompleteListener> listeners = new CopyOnWriteArraySet<ThreadCompleteListener>();

	private boolean useRunnable = false;

	/**
	 * The error that stopped the execution
	 */
	private Throwable error;

	/**
	 * Interface to be implemented by the listeners
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.5.3
	 */
	public interface ThreadCompleteListener {
		public void completed(final NotifyingThread thread);
	}

	public final void addListener(final ThreadCompleteListener listener) {
		listeners.add(listener);
	}

	public final void removeListener(final ThreadCompleteListener listener) {
		listeners.remove(listener);
	}

	private final void notifyListeners() {
		for (ThreadCompleteListener listener : listeners) {
			listener.completed(this);
		}
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
	public final void run() {
		try {
			endTime = 0;
			startTime = System.currentTimeMillis();
			error = null;
			log.debug("Thread {} started", getName());
			if (useRunnable)
				super.run();
			else
				doRun();
		} catch (Throwable t) {
			error = t;
			log.debug("Thread {} ended in error after {}", getName(), TimeDiff.printDuration(getElapsedTime()), t);
		} finally {
			endTime = System.currentTimeMillis();
			notifyListeners();
		}

	}

	public NotifyingThread() {
		super();
	}

	public NotifyingThread(String name) {
		super(name);
	}

	public NotifyingThread(ThreadGroup group, String name) {
		super(group, name);
	}

	public NotifyingThread(Runnable target, String name) {
		super(target, name);
		useRunnable = true;
	}

	/**
	 * Concrete implementations should override this method to implement their
	 * own processing.
	 * 
	 * @throws Throwable whatever error that happens during the elaboration
	 */
	public void doRun() throws Throwable {
	}

	public Throwable getError() {
		return error;
	}
}
