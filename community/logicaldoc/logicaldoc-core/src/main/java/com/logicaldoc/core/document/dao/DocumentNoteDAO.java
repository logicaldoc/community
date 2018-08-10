package com.logicaldoc.core.document.dao;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.History;

/**
 * DAO for <code>DocumentNote</code> handling.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public interface DocumentNoteDAO extends PersistentObjectDAO<DocumentNote> {

	public boolean store(DocumentNote note, History transaction);

	/**
	 * This method finds the list of document note regarding a document with the
	 * given ID.
	 * 
	 * @param docId ID of the document.
	 * @return The list of document note.
	 */
	public List<DocumentNote> findByDocId(long docId);

	/**
	 * Deletes all content annotations(notes on pages)
	 * 
	 * @param docId The identifier of the document
	 */
	public void deleteContentAnnotations(long docId);

	/**
	 * This method finds the list of document notes regarding posted by a
	 * specific user.
	 * 
	 * @param userId ID of the user
	 * @return The list of document notes ordered by descending date
	 */
	public List<DocumentNote> findByUserId(long userId);
}