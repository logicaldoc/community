package com.logicaldoc.core.communication;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.History;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.Context;
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

	private Set<EventListener> listeners = new HashSet<EventListener>();

	private ContextProperties config;

	// Maintain a fifos for the history IDs. Key is the class name, value is a
	// FIFO queue
	private Map<String, Queue<Long>> fifos = new HashMap<String, Queue<Long>>();

	public static EventCollector get() {
		return (EventCollector) Context.get().getBean(EventCollector.class);
	}

	public void addListener(EventListener listener) {
		if (!listeners.contains(listener))
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
	private boolean rememberHistory(History history) {
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
	 * 
	 * @param history the history to notify
	 */
	public void newEvent(History history) {
		if (!isEnabled()) {
			log.debug("Aspect {} not enabled", ASPECT);
			return;
		}

		if (!history.isNotifyEvent())
			return;

		if (!rememberHistory(history))
			return;

		if (history.getDocId() != null && history.getDocument() == null) {
			DocumentDAO docDao = (DocumentDAO) com.logicaldoc.util.Context.get().getBean(DocumentDAO.class);
			try {
				history.setDocument(docDao.findById(history.getDocId()));
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		} else if (history.getDocument() != null) {
			/*
			 * Do not use the original document because to avoid interactions
			 * with Hibernate session.
			 */
			Document clone = new Document(history.getDocument());
			// Restore some attributes skipped by the clone method
			clone.setCustomId(history.getDocument().getCustomId());
			clone.setStatus(history.getDocument().getStatus());
			history.setDocument(clone);
		}

		Runnable notifier = new Runnable() {
			@Override
			public void run() {
				log.debug("Notify history {}", history);
				for (EventListener listener : listeners) {
					listener.newEvent(history);
				}
				log.debug("Finished notification of history {}", history);
			}
		};

		ThreadPools pools = (ThreadPools) Context.get().getBean(ThreadPools.class);
		pools.execute(notifier, "EventCollector");
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