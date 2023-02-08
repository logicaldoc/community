package com.logicaldoc.gui.common.client.observer;

import java.util.HashSet;
import java.util.Set;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;

/**
 * Implements the Observer pattern to distribute events on the documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 *
 */
public class DocumentController {

	private static DocumentController instance = new DocumentController();

	private Set<DocumentObserver> observers = new HashSet<>();

	/**
	 * The currently selected document
	 */
	private GUIDocument currentDocument = null;

	/**
	 * If the current document under editing
	 */
	private boolean editing = false;

	private DocumentController() {
	}

	public static DocumentController get() {
		return instance;
	}

	public synchronized void addObserver(DocumentObserver observer) {
		if (observer != null && !observers.contains(observer)) {
			observers.add(observer);
		}
	}

	public synchronized void removeObserver(DocumentObserver observer) {
		if (observer != null && observers.contains(observer)) {
			observers.remove(observer);
		}
	}

	public void selected(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentSelected(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void deleted(GUIDocument[] documents) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentsDeleted(documents);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void moved(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentMoved(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void modified(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentModified(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void stored(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentStored(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void checkedIn(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentCheckedIn(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void checkedOut(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentCheckedOut(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void locked(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentLocked(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public void unlocked(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentUnlocked(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public synchronized void beginEditing(GUIDocument document) {
		currentDocument = document;
		editing = true;
		if (document.getStatus() == GUIDocument.DOC_UNLOCKED && Session.get().getConfigAsBoolean("gui.onedit.lock")) {
			DocumentService.Instance.get().lock(new long[] { document.getId() }, null, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.error(I18N.message("cannotlockdoc"), null, null);
				}

				@Override
				public void onSuccess(Void arg) {
					document.setStatus(GUIDocument.DOC_LOCKED);
					document.setLockUserId(Session.get().getUser().getId());
					document.setLockUser(Session.get().getUser().getFullName());
					notifyBeginEditng(document);
				}
			});
		} else {
			notifyBeginEditng(document);
		}
	}

	private void notifyBeginEditng(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentBeginEditing(document);
				} catch (Throwable t) {
					// Nothing to do
				}
		}
	}

	public synchronized void cancelEditing(GUIDocument document) {
		if (document != null && isEditing(document) && Session.get().getConfigAsBoolean("gui.onedit.lock")) {
			DocumentService.Instance.get().unlock(new long[] { document.getId() }, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.error(I18N.message("cannotunlockdoc"), null, null);
				}

				@Override
				public void onSuccess(Void arg0) {
					document.setStatus(GUIDocument.DOC_UNLOCKED);
					document.setLockUserId(null);
					document.setLockUser(null);
					notifyCancelEditng(document);
				}
			});
		} else {
			notifyCancelEditng(document);
		}
	}

	private void notifyCancelEditng(GUIDocument document) {
		synchronized (observers) {
			for (DocumentObserver observer : observers)
				try {
					observer.onDocumentCancelEditing(document);
				} catch (Throwable t) {
					// Nothing to do
				}
			editing = false;
		}
	}

	public boolean isEditing(GUIDocument document) {
		return currentDocument != null && document != null && currentDocument.getId() == document.getId() && editing;
	}

	public void setCurrentDocument(GUIDocument document) {
		this.currentDocument = document;
		selected(document);
	}

	public GUIDocument getCurrentDocument() {
		return currentDocument;
	}

	public boolean isCurrentDocument(GUIDocument document) {
		return currentDocument != null && document != null && currentDocument.getId() == document.getId();
	}
}