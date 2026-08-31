package com.logicaldoc.core.document;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;

/**
 * DAO for <code>DocumentLink</code> handling.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
public interface DocumentLinkDAO extends PersistentObjectDAO<DocumentLink> {

	/**
	 * This method finds the list of document link in which there is a document
	 * with the given ID
	 * 
	 * @param docId ID of the document
	 * 
	 * @return The list of document link
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentLink> findByDocId(long docId) throws PersistenceException;

	/**
	 * This method finds the list of document link, filtered by the given link
	 * type, in which there is a document with the given ID
	 * 
	 * @param docId ID of the document
	 * @param type Type of each document link in the return list
	 * 
	 * @return The list of document link
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentLink> findByDocId(long docId, String type) throws PersistenceException;

	/**
	 * Find a document link using its alternate keys
	 * 
	 * @param docId1 identifier of the first document
	 * @param docId2 identifier of the second
	 * @param type the document type(<b>null</b> or <b>pdf</b>
	 * 
	 * @return The found instance
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public DocumentLink findByDocIdsAndType(long docId1, long docId2, String type) throws PersistenceException;
}