package com.logicaldoc.core.document;

import java.util.Map;

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
	 * @throws Exception
	 */
	public void beforeStore(Document document, History transaction, Map<String, Object> dictionary) throws Exception;

	/**
	 * Called after a document is stored in the database
	 * 
	 * @param document The document to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws Exception
	 */
	public void afterStore(Document document, History transaction, Map<String, Object> dictionary) throws Exception;

	/**
	 * Called before a document is checked in
	 * 
	 * @param document The document to be checked in
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws Exception
	 */
	public void beforeCheckin(Document document, History transaction, Map<String, Object> dictionary) throws Exception;

	/**
	 * Called after a document is checked in
	 * 
	 * @param document The document to be checked in
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws Exception
	 */
	public void afterCheckin(Document document, History transaction, Map<String, Object> dictionary) throws Exception;
}