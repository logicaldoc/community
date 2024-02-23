package com.logicaldoc.core.document;

import java.util.Collection;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;

/**
 * DAO for <code>DocumentNote</code> handling.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public interface DocumentNoteDAO extends PersistentObjectDAO<DocumentNote> {

	/**
	 * Stores a note and saves the document's history
	 * 
	 * @param note the note
	 * @param transaction session informations
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void store(DocumentNote note, DocumentHistory transaction) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with
	 * the given ID
	 * 
	 * @param docId ID of the document.
	 * @param fileVersion indicates a specific file version, optional
	 * 
	 * @return The list of document note
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByDocId(long docId, String fileVersion) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with
	 * the given ID and optionally filter on the type
	 * 
	 * @param docId ID of the document
	 * @param fileVersion indicates a specific file version, optional
	 * @param type note type, optional
	 * 
	 * @return The list of document note
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByDocIdAndType(long docId, String fileVersion, String type)
			throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with
	 * the given ID and optionally filter on a collection of types
	 * 
	 * @param docId ID of the document
	 * @param fileVersion indicates a specific file version, optional
	 * @param types collection of admitted note types, optional
	 * 
	 * @return The list of document note
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByDocIdAndTypes(long docId, String fileVersion, Collection<String> types)
			throws PersistenceException;

	/**
	 * Copies all the notes not associated to a specific page from a given file
	 * version to another
	 * 
	 * @param docId The document ID
	 * @param oldFileVersion the old version
	 * @param newFileVersion the version to copy to
	 *
	 * @return Number of copied notes
	 * 
	 * @throws PersistenceException If an error occurs in the database
	 */
	public int copyAnnotations(long docId, String oldFileVersion, String newFileVersion) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding posted by a
	 * specific user
	 * 
	 * @param userId ID of the user
	 * 
	 * @return The list of document notes ordered by descending date
	 * 
	 * @throws PersistenceException Errorin the database
	 */
	public List<DocumentNote> findByUserId(long userId) throws PersistenceException;
}