package com.logicaldoc.gui.common.client.observer;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.log.GuiLog;

/**
 * Implements the Observer pattern to distribute events on the folders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class FolderController {

	private static FolderController instance = new FolderController();

	private Set<FolderObserver> observers = new HashSet<FolderObserver>();

	private FolderController() {
	}

	public static FolderController get() {
		return instance;
	}

	public synchronized void addObserver(FolderObserver observer) {
		if (observer != null && !observers.contains(observer)) {
			observers.add(observer);
		}
	}

	public synchronized void removeObserver(FolderObserver observer) {
		if (observer != null && observers.contains(observer)) {
			observers.remove(observer);
		}
	}

	public void selected(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderSelected(folder);
				} catch (Throwable t) {
					GuiLog.error(t.getMessage(), null, t);
				}
		}
	}

	public void modified(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderChanged(folder);
				} catch (Throwable t) {
				}
		}
	}

	public void deleted(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderDeleted(folder);
				} catch (Throwable t) {
				}
		}
	}

	public void created(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderCreated(folder);
				} catch (Throwable t) {
				}
		}
	}

	public void moved(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderMoved(folder);
				} catch (Throwable t) {
				}
		}
	}
}