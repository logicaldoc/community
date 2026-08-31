package com.logicaldoc.core.document;

import java.util.Map;

import com.logicaldoc.core.PersistenceException;

/**
 * This interface defines hooks called before and after a particular event
 * occurs on the specified document.
 * <p>
 * Each methods has access to a dictionary map that can be used through the
 * execution pipeline in order to carry needed informations among all listeners.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public interface DocumentListener {
	/**
	 * Called before a document is stored in the database
	 * 
	 * @param document The document to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void beforeStore(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException;

	/**
	 * Called after a document is stored in the database
	 * 
	 * @param document The document to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void afterStore(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException;

	/**
	 * Called before a document is checked in
	 * 
	 * @param document The document to be checked in
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void beforeCheckin(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException;

	/**
	 * Called after a document is checked in
	 * 
	 * @param document The document to be checked in
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void afterCheckin(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException;

	/**
	 * Called after an event has been stored
	 * 
	 * @param document The document the event is relative to
	 * @param event The saved event
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raised if something went wrong
	 */
	public void afterSaveHistory(Document document, DocumentHistory event, Map<String, Object> dictionary)
			throws PersistenceException;
}