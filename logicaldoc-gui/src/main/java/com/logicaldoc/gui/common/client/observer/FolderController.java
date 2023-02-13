package com.logicaldoc.gui.common.client.observer;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.WindowUtils;

/**
 * Implements the Observer pattern to distribute events on the folders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class FolderController {

	private static FolderController instance = new FolderController();

	private Set<FolderObserver> observers = new HashSet<>();

	private GUIFolder currentFolder;
	
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
		this.currentFolder = folder;
		
		WindowUtils.setTitle(Session.get().getInfo(), folder!=null && folder.getPathExtended() != null ? folder.getPathExtended() : "");

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
					// Nothing to do
				}
		}
	}

	public void deleted(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderDeleted(folder);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void created(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderCreated(folder);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void moved(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderMoved(folder);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}
	
	public void beginEditing(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderBeginEditing(folder);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void cancelEditing(GUIFolder folder) {
		synchronized (observers) {
			for (FolderObserver observer : observers)
				try {
					observer.onFolderCancelEditing(folder);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public GUIFolder getCurrentFolder() {
		return currentFolder;
	}
}