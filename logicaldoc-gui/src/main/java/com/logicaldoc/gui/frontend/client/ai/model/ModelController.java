package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.HashSet;
import java.util.Set;

/**
 * Implements the Observer pattern to distribute events on the models
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelController {

	private static ModelController instance = new ModelController();

	private Set<ModelObserver> observers = new HashSet<>();

	private ModelController() {
	}

	public static ModelController get() {
		return instance;
	}

	public synchronized void addObserver(ModelObserver observer) {
		if (observer != null && !observers.contains(observer)) {
			observers.add(observer);
		}
	}

	public synchronized void removeObserver(ModelObserver observer) {
		if (observer != null && observers.contains(observer)) {
			observers.remove(observer);
		}
	}

	public void changed(GUIModel model) {
		synchronized (observers) {
			for (ModelObserver observer : observers)
				try {
					observer.onModelChanged(model);
				} catch (Exception t) {
					// Nothing to do
				}
		}
	}
}