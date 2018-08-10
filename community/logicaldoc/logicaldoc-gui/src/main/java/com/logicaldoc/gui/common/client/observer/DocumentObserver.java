package com.logicaldoc.gui.common.client.observer;

import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * Listener on documents events
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public interface DocumentObserver {

	/**
	 * Invoked when a document is selected
	 */
	public void onDocumentSelected(GUIDocument document);

	/**
	 * Invoked after the document's properties has been changed
	 */
	public void onDocumentModified(GUIDocument document);

	/**
	 * Invoked after the document has been stored
	 */
	public void onDocumentStored(GUIDocument document);

	/**
	 * Invoked after the document has been stored
	 */
	public void onDocumentMoved(GUIDocument document);

	/**
	 * Invoked after the document's checkin
	 */
	public void onDocumentCheckedIn(GUIDocument document);

	/**
	 * Invoked after the document's checkout
	 */
	public void onDocumentCheckedOut(GUIDocument document);

	/**
	 * Invoked after the document is locked
	 */
	public void onDocumentLocked(GUIDocument document);

	/**
	 * Invoked after the document is unlocked
	 */
	public void onDocumentUnlocked(GUIDocument document);

	/**
	 * Invoked after the document has been deleted
	 */
	public void onDocumentsDeleted(GUIDocument[] documents);
}