package com.logicaldoc.core.document.dao;

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

	public boolean store(DocumentNote note, DocumentHistory transaction) throws PersistenceException;

	/**
	 * This method finds the list of document note regarding a document with the
	 * given ID
	 * 
	 * @param docId ID of the document.
	 * @param fileVersion indicates a specific file version, optional
	 * 
	 * @return The list of document note
	 */
	public List<DocumentNote> findByDocId(long docId, String fileVersion);

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