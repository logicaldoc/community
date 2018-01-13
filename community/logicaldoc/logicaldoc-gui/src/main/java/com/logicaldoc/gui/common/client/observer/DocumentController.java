package com.logicaldoc.gui.common.client.observer;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * Implements the Observer pattern to distribute events on the documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 *
 */
public class DocumentController {

	private static DocumentController instance = new DocumentController();

	private List<DocumentObserver> observers = new ArrayList<DocumentObserver>();

	private DocumentController() {
	}

	public static DocumentController get() {
		return instance;
	}

	public synchronized void addObserver(DocumentObserver observer) {
		if (observer != null && !observers.contains(observer))
			observers.add(observer);
	}

	public synchronized void removeObserver(DocumentObserver observer) {
		if (observer != null && observers.contains(observer))
			observers.remove(observer);
	}

	public void selected(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentSelected(document);
			} catch (Throwable t) {
			}
	}

	public void deleted(GUIDocument[] documents) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentsDeleted(documents);
			} catch (Throwable t) {
			}
	}

	public void moved(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentMoved(document);
			} catch (Throwable t) {
			}
	}

	public void modified(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentModified(document);
			} catch (Throwable t) {
			}
	}

	public void stored(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentStored(document);
			} catch (Throwable t) {
			}
	}

	public void checkedIn(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentCheckedIn(document);
			} catch (Throwable t) {
			}
	}

	public void checkedOut(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentCheckedOut(document);
			} catch (Throwable t) {
			}
	}

	public void locked(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentLocked(document);
			} catch (Throwable t) {
			}
	}

	public void unlocked(GUIDocument document) {
		for (DocumentObserver observer : observers)
			try {
				observer.onDocumentUnlocked(document);
			} catch (Throwable t) {
			}
	}
}