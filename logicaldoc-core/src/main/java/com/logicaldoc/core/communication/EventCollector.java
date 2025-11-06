package com.logicaldoc.core.communication;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.document.AbstractDocumentHistory;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.spring.Context;

/**
 * A collector of events that can distribute them to a set of listeners
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
@Component("eventCollector")
public class EventCollector {

	private static final int FIFO_SIZE = 1000;

	private static final String ASPECT = "eventsGathering";

	private static final Logger log = LoggerFactory.getLogger(EventCollector.class);

	private Set<EventListener> listeners = new HashSet<>();

	public EventCollector() {
		super();
	}

	// Maintain a fifos for the history IDs. Key is the class name, value is a
	// FIFO queue
	private Map<String, Queue<Long>> fifos = new HashMap<>();

	public static EventCollector get() {
		return Context.get(EventCollector.class);
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
			fifo = new CircularFifoQueue<>(FIFO_SIZE);
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

		if (history instanceof AbstractDocumentHistory adh) {
			if (adh.getDocId() != null && adh.getDocument() == null) {
				try {
					adh.setDocument(DocumentDAO.get().findById(adh.getDocId()));
				} catch (PersistenceException e) {
					log.error(e.getMessage(), e);
				}
			} else if (adh.getDocument() != null && adh.getDocument() instanceof Document doc) {
				/*
				 * Do not use the original document because to avoid
				 * interactions with Hibernate session.
				 */
				Document clone = new Document(doc);
				// Restore some attributes skipped by the clone method
				clone.setCustomId(adh.getDocument().getCustomId());
				clone.setStatus(adh.getDocument().getStatus());
				adh.setDocument(clone);
			}
		}

		ThreadPools pools = Context.get(ThreadPools.class);
		pools.execute(() -> {
			log.debug("Notify history {}", history);
			for (EventListener listener : listeners) {
				listener.newEvent(history);
			}
			log.debug("Finished notification of history {}", history);
			return null;
		}, "EventCollector");
	}

	public static boolean isEnabled() {
		return RunLevel.current().aspectEnabled(ASPECT);
	}
}