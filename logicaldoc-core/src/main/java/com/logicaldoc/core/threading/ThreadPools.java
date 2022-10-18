package com.logicaldoc.core.threading;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

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
 * </ul>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class ThreadPools {

	private static String TYPE_DEFAULT = "default";

	private static String TYPE_SCHEDULED = "scheduled";

	private static Logger log = LoggerFactory.getLogger(ThreadPools.class);

	private Map<String, ExecutorService> pools = new HashMap<String, ExecutorService>();

	private ContextProperties config;

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public static ThreadPools get() {
		return (ThreadPools) Context.get().getBean(ThreadPools.class);
	}

	/**
	 * Gets a new thread pool and in case it does not exist a new one will be
	 * created and then cached.
	 * 
	 * @param name the name of the new pool
	 * 
	 * @return the created pool
	 */
	public synchronized ExecutorService getPool(String name) {
		ExecutorService pool = pools.get(name);

		if (pool != null) {
			if (pools.get(name).isShutdown())
				throw new RuntimeException(name + " pool was shutdown");
		} else {
			int core = config.getInt("threadpool." + name + ".core", 5);
			int max = config.getInt("threadpool." + name + ".max", 10);
			int keepalive = config.getInt("threadpool." + name + ".keepalive", 5);
			String type = config.getString("threadpool." + name + ".type", TYPE_SCHEDULED);

			if (TYPE_DEFAULT.equals(type))
				pool = new ScheduledThreadPoolExecutor(core, new NamedThreadFactory(name));
			else
				pool = new ThreadPoolExecutor(core, max, keepalive, TimeUnit.SECONDS,
						new LinkedBlockingQueue<Runnable>());
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
	public void schedule(Runnable task, String poolName, long delay) {
		ExecutorService pool = getPool(poolName);
		if (pool instanceof ScheduledExecutorService)
			((ScheduledExecutorService) pool).schedule(task, delay, TimeUnit.MILLISECONDS);
		else {
			log.debug("Pool {} does not support scheduling so the task has been started immediately", poolName);
			execute(task, poolName);
		}
	}

	/**
	 * Executes a task in the given pool.
	 * 
	 * @param task The task to execute
	 * @param poolName The name of the pool
	 */
	public void execute(Runnable task, String poolName) {
		ExecutorService pool = getPool(poolName);
		pool.execute(task);
	}

	/**
	 * Shuts down all the pools
	 */
	public void shutdown() {
		log.info("Shutting down {} thread pools", pools.size());
		for (String name : pools.keySet()) {
			log.info("Killing all the threads of pool {}", name);

			ExecutorService pool = pools.get(name);
			pool.shutdownNow();
			try {
				pool.awaitTermination(3, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
	}
}