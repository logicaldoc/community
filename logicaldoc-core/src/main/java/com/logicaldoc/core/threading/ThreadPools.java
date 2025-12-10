package com.logicaldoc.core.threading;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;

/**
 * A factory for thread pools. Each pool has a name and the details are taken
 * from the configuration in the following as follows:
 * 
 * <ul>
 * <li>threadpool.<b>pool_name</b>.core: the number of threads to keep in the
 * pool, even if they are idle (defalut value: 5)</li>
 * <li>threadpool.<b>pool_name</b>.max: the maximum number of threads to allow
 * in the pool (defalut value: 10)</li>
 * <li>threadpool.<b>pool_name</b>.keepalive: this is the maximum time(in
 * seconds) that excess idle threads will wait for new tasks before terminating
 * (defalut value: 5)</li>
 * <li>threadpool.<b>pool_name</b>.type: this is the type default or
 * scheduled(default value: default), default means</li>
 * </ul>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
@Component("threadPools")
public class ThreadPools {

	private static final String THREADPOOL = "threadpool.";

	private static final String TYPE_DEFAULT = "default";

	private static final Logger log = LoggerFactory.getLogger(ThreadPools.class);

	private Map<String, ThreadPoolExecutor> pools = new HashMap<>();

	/**
	 * Map of those active futures that extends {@link MonitoredCallable}
	 */
	private Map<String, Future<?>> monitoredFutures = new HashMap<>();

	@Resource(name = "ContextProperties")
	protected ContextProperties config;

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static ThreadPools get() {
		return Context.get(ThreadPools.class);
	}

	/**
	 * Gets a new thread pool and in case it does not exist a new one will be
	 * created and then cached.
	 * 
	 * @param name the name of the new pool
	 * 
	 * @return the created pool
	 * 
	 * @throws ThreadPoolNotAvailableException Raised in case the pool has been
	 *         shutdown
	 */
	public synchronized ThreadPoolExecutor getPool(String name) throws ThreadPoolNotAvailableException {
		ThreadPoolExecutor pool = pools.get(name);

		if (pool != null) {
			if (pools.get(name).isShutdown())
				throw new ThreadPoolNotAvailableException(name);
		} else {
			int core = config.getInt(THREADPOOL + name + ".core", 5);
			int max = config.getInt(THREADPOOL + name + ".max", 10);
			int keepalive = config.getInt(THREADPOOL + name + ".keepalive", 5);
			String type = config.getString(THREADPOOL + name + ".type", TYPE_DEFAULT);

			if (TYPE_DEFAULT.equals(type))
				pool = new ScheduledThreadPoolExecutor(core, new NamedThreadFactory(name));
			else
				pool = new ThreadPoolExecutor(core, max, keepalive, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
			pools.put(name, pool);
		}

		return pool;
	}

	/**
	 * Schedule the execution of a task in a thread pool.
	 * 
	 * @param task The task to execute
	 * @param poolName The name of the pool
	 * @param delay a delay expressed in milliseconds
	 */
	public <T> Future<T> schedule(Callable<T> task, String poolName, long delay) {
		try {
			ExecutorService pool = getPool(poolName);
			Future<T> future;
			if (pool instanceof ScheduledExecutorService executorService)
				future = executorService.schedule(task, delay, TimeUnit.MILLISECONDS);
			else {
				log.debug("Pool {} does not support scheduling so the task has been started immediately", poolName);
				future = execute(task, poolName);
			}

			cacheFuture(task, future);

			return future;
		} catch (ThreadPoolNotAvailableException e) {
			log.error(e.getMessage());
			return null;
		}
	}

	/**
	 * Executes a task in the given pool.
	 * 
	 * @param task The task to execute
	 * @param poolName The name of the pool
	 */
	public <T> Future<T> execute(Callable<T> task, String poolName) {
		try {
			ExecutorService pool = getPool(poolName);
			Future<T> future = pool.submit(task);

			cacheFuture(task, future);

			return future;
		} catch (ThreadPoolNotAvailableException e) {
			log.error(e.getMessage());
			return null;
		}
	}

	private <T> void cacheFuture(Callable<?> task, Future<T> future) {
		// clear from completed threads
		monitoredFutures.entrySet().removeIf(entry -> entry.getValue().isDone() || entry.getValue().isCancelled());

		if (task instanceof MonitoredCallable<?> monitored)
			monitoredFutures.put(monitored.getName(), future);
	}

	/**
	 * Kills a task
	 * 
	 * @param taskName Name of the thread
	 */
	public void kill(String taskName) {
		Future<?> task = monitoredFutures.get(taskName);
		if (task != null) {
			task.cancel(true);
		}
	}

	/**
	 * Gets the running status of a task
	 * 
	 * @param taskName Name of the thread
	 * 
	 * @return If it is running
	 */
	public boolean isRunning(String taskName) {
		Future<?> task = monitoredFutures.get(taskName);
		if (task != null) {
			return task.isDone();
		} else
			return false;
	}

	/**
	 * Shuts down all the pools
	 */
	@PreDestroy
	public void shutdown() {
		log.info("Shutting down {} thread pools", pools.size());
		for (Map.Entry<String, ThreadPoolExecutor> entry : pools.entrySet()) {
			log.info("Killing all the threads of pool {}", entry.getKey());

			ExecutorService pool = entry.getValue();
			pool.shutdownNow();
			try {
				pool.awaitTermination(3, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
	}
}