package com.logicaldoc.gui.common.client.controllers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;

/**
 * Implements the Observer pattern to distribute events on the reading requests
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class ReadingRequestController {

	private static ReadingRequestController instance = new ReadingRequestController();

	private Set<ReadingRequestObserver> observers = new HashSet<>();

	// Map docId - list of unconfirmed readings
	private Map<Long, List<GUIReadingRequest>> unconfirmedReadings = new HashMap<>();

	private ReadingRequestController() {
	}

	public static ReadingRequestController get() {
		return instance;
	}

	public synchronized void addObserver(ReadingRequestObserver observer) {
		if (observer != null && !observers.contains(observer)) {
			observers.add(observer);
		}
	}

	public synchronized void removeObserver(ReadingRequestObserver observer) {
		if (observer != null && observers.contains(observer)) {
			observers.remove(observer);
		}
	}

	public void addUnconfirmedReadings(GUIReadingRequest[] readings) {
		if (readings == null || readings.length == 0)
			return;

		for (GUIReadingRequest guiReading : readings) {
			List<GUIReadingRequest> docReadings = unconfirmedReadings.get(guiReading.getDocId());
			if (docReadings == null) {
				docReadings = new ArrayList<>();
				unconfirmedReadings.put(guiReading.getDocId(), docReadings);
			}

			if (!docReadings.contains(guiReading))
				docReadings.add(guiReading);
		}

		synchronized (observers) {
			for (ReadingRequestObserver observer : observers)
				try {
					observer.onNewReadingRequests(readings);
				} catch (Exception t) {
					// Nothing to do
				}
		}
	}

	public boolean isReadingConfirmRequired(long docId) {
		return unconfirmedReadings.containsKey(docId) && !unconfirmedReadings.get(docId).isEmpty();
	}

	public List<GUIReadingRequest> getUnconfirmedReadings(long docId) {
		return unconfirmedReadings.get(docId);
	}

	public long[] getUnconfirmedReadingIds(long docId) {
		long[] readingIds = new long[0];
		if (isReadingConfirmRequired(docId)) {
			List<Long> values = unconfirmedReadings.get(docId).stream().map(r -> r.getId())
					.collect(Collectors.toList());
			readingIds = values.stream().mapToLong(l -> l).toArray();
		}
		return readingIds;
	}

	public int countUnconfirmedReadings() {
		int count = 0;
		for (List<GUIReadingRequest> readings : unconfirmedReadings.values())
			count += readings.size();
		return count;
	}

	public void confirmReading(long docId) {
		unconfirmedReadings.remove(docId);
		synchronized (observers) {
			for (ReadingRequestObserver observer : observers)
				try {
					observer.onConfirmReading(docId);
				} catch (Exception t) {
					// Nothing to do
				}
		}
	}
}