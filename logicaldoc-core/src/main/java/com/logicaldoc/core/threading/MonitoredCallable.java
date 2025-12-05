package com.logicaldoc.core.threading;

import java.util.concurrent.Callable;

import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.time.TimeDiff;

/**
 * A callable that is identified by a unique name and whose running statyus is
 * monitorable
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 *
 * @param <V> Type of callable
 */
public abstract class MonitoredCallable<V> implements Callable<V> {

	private static final Logger log = LoggerFactory.getLogger(MonitoredCallable.class);

	private String name;

	private boolean running = true;

	public MonitoredCallable(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public boolean isRunning() {
		return running;
	}

	@Override
	public V call() throws Exception {
		log.info("Callable {} is starting", name);

		StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		running = true;
		try {
			return internalCall();
		} finally {
			stopWatch.stop();
			running = false;
			log.info("Callable {} completed in {}", name, TimeDiff.printDuration(stopWatch.getTime()));
		}
	}

	@Override
	public String toString() {
		return name;
	}

	protected abstract V internalCall() throws Exception;
}