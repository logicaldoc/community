package com.logicaldoc.core.document.dao;

import java.util.Collection;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentNote;

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
	 * @throws PersistenceException raised in case of database errors
	 */
	public void store(DocumentNote note, DocumentHistory transaction) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with the
	 * given ID
	 * 
	 * @param docId ID of the document.
	 * @param fileVersion indicates a specific file version, optional
	 * 
	 * @return The list of document note
	 */
	public List<DocumentNote> findByDocId(long docId, String fileVersion);

	/**
	 * This method finds the list of document notes regarding a document with the
	 * given ID and optionally filter on the type
	 * 
	 * @param docId ID of the document
	 * @param fileVersion indicates a specific file version, optional
	 * @param type note type, optional
	 * 
	 * @return The list of document note
	 */
	public List<DocumentNote> findByDocIdAndType(long docId, String fileVersion, String type);
	
	/**
	 * This method finds the list of document notes regarding a document with the
	 * given ID and optionally filter on a collection of types
	 * 
	 * @param docId ID of the document
	 * @param fileVersion indicates a specific file version, optional
	 * @param types collection of admitted note types, optional
	 * 
	 * @return The list of document note
	 */
	public List<DocumentNote> findByDocIdAndTypes(long docId, String fileVersion, Collection<String> types);
	
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
	 */
	public List<DocumentNote> findByUserId(long userId);
}