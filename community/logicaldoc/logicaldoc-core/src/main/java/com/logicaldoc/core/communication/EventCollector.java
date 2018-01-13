package com.logicaldoc.core.communication;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.document.AbstractHistory;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.util.config.ContextProperties;

/**
 * A collector of events that can distribute them to a set of listeners
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class EventCollector {

	private static final int FIFO_SIZE = 1000;

	private final static String ASPECT = "eventsGathering";

	private static Logger log = LoggerFactory.getLogger(EventCollector.class);

	private static EventCollector instance;

	private Set<EventListener> listeners = new HashSet<EventListener>();

	private ContextProperties config;

	private ExecutorService threadPoolExecutor = null;

	// Maintain a fifos for the history IDs. Key is the class name, value is a
	// FIFO queue
	private Map<String, Queue<Long>> fifos = new HashMap<String, Queue<Long>>();

	public static EventCollector get() {
		return instance;
	}

	public void start() {
		EventCollector.instance = this;

		if (threadPoolExecutor != null)
			stop();

		if (!isEnabled()) {
			log.warn("Aspect {} not enabled", ASPECT);
			return;
		}

		int corePoolSize = 5;

		int maxPoolSize = config.getInt("eventcollector.maxpool", 20);

		long keepAliveTime = config.getInt("eventcollector.keepalive", 5);

		threadPoolExecutor = new ThreadPoolExecutor(corePoolSize, maxPoolSize, keepAliveTime, TimeUnit.SECONDS,
				new LinkedBlockingQueue<Runnable>());
		log.info("Installed message collector thread executor");
	}

	public void stop() {
		if (threadPoolExecutor != null) {
			threadPoolExecutor.shutdownNow();
			try {
				threadPoolExecutor.awaitTermination(3, TimeUnit.SECONDS);
			} catch (InterruptedException e) {
			}
		}
	}

	public void addListener(EventListener listener) {
		listeners.add(listener);
	}

	public void removeListener(EventListener listener) {
		listeners.remove(listener);
	}

	/**
	 * Puts the history in the relative FIFO
	 * 
	 * @param history
	 * @return true if it was not remembered already, false otherwise
	 */
	private boolean rememberHistory(AbstractHistory history) {
		Queue<Long> fifo = fifos.get(history.getClass().getName());
		if (fifo == null) {
			fifo = new CircularFifoQueue<Long>(FIFO_SIZE);
			fifos.put(history.getClass().getName(), fifo);
		}

		if (fifo.contains(history.getId()))
			return false;
		else {
			fifo.add(history.getId());
			return true;
		}
	}

	/**
	 * Notifies all the listeners in another thread
	 */
	public void newEvent(AbstractHistory history) {
		if (!isEnabled())
			return;

		if (!history.isNotifyEvent())
			return;

		if (!rememberHistory(history))
			return;

		if (history.getDocId() != null && history.getDocument() == null) {
			DocumentDAO docDao = (DocumentDAO) com.logicaldoc.util.Context.get().getBean(DocumentDAO.class);
			history.setDocument(docDao.findById(history.getDocId()));
		} else if (history.getDocument() != null) {
			/*
			 * Do not use the original document beacause to avoid interactions
			 * with Hibernate session.
			 */
			try {
				Document clone = (Document) history.getDocument().clone();
				history.setDocument(clone);
			} catch (CloneNotSupportedException e) {
				log.error(e.getMessage());
			}
		}

		Runnable notifier = new Runnable() {
			@Override
			public void run() {
				log.debug("Notify history " + history);
				for (EventListener listener : listeners) {
					listener.newEvent(history);
				}
				log.debug("Finished notification of history " + history);
			}
		};

		threadPoolExecutor.execute(notifier);
	}

	public ContextProperties getConfig() {
		return config;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public static boolean isEnabled() {
		return RunLevel.current().aspectEnabled(ASPECT);
	}
}