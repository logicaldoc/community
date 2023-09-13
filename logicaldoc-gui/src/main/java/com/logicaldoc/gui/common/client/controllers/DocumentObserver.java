package com.logicaldoc.gui.common.client.controllers;

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
	 * 
	 * @param document the document that has been selected
	 */
	public void onDocumentSelected(GUIDocument document);

	/**
	 * Invoked after the document's properties has been changed
	 * 
	 * @param document the document that has been modified
	 */
	public void onDocumentModified(GUIDocument document);
	
	/**
	 * Invoked after the document's properties are being edited
	 * 
	 * @param document the document that is being edited
	 */
	public void onDocumentBeginEditing(GUIDocument document);	
	
	/**
	 * Invoked after the document's properties are being edited
	 * 
	 * @param document the document that is being edited
	 */
	public void onDocumentCancelEditing(GUIDocument document);
	

	/**
	 * Invoked after the document has been stored
	 * 
	 * @param document the document that has been stored
	 */
	public void onDocumentStored(GUIDocument document);

	/**
	 * Invoked after the document has been stored
	 * 
	 * @param document the document that has been moved
	 */
	public void onDocumentMoved(GUIDocument document);

	/**
	 * Invoked after the document's checkin
	 * 
	 * @param document the document that has been checked in
	 */
	public void onDocumentCheckedIn(GUIDocument document);

	/**
	 * Invoked after the document's checkout
	 * 
	 * @param document the document that has been checked out
	 */
	public void onDocumentCheckedOut(GUIDocument document);

	/**
	 * Invoked after the document is locked
	 * 
	 * @param document the document that has been locked
	 */
	public void onDocumentLocked(GUIDocument document);

	/**
	 * Invoked after the document is unlocked
	 * 
	 * @param document the document that has been unlocked
	 */
	public void onDocumentUnlocked(GUIDocument document);

	/**
	 * Invoked after the document has been deleted
	 * 
	 * @param documents the documents that have been deleted
	 */
	public void onDocumentsDeleted(GUIDocument[] documents);
}